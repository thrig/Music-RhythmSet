#!perl

use 5.24.0;
use Test::Most tests => 62;
my $deeply = \&eq_or_diff;

use Music::RhythmSet;
use Scalar::Util qw(refaddr);

my $set = Music::RhythmSet->new;

$deeply->( $set->voices, [] );

$set->stash("foo");
is( $set->stash, "foo" );

# to capture results from the next callback
my $foo;
my $measure = 0;
my $next_set;
my $pattern = [];

$set->add(
    { pattern => [qw/0 1/], ttl => 3, id => 42 },
    {   pattern => [qw/1 0/],
        ttl     => 1,
        next    => sub {
            my ( $self, %param ) = @_;
            $foo      = $param{foo};
            $measure  = $param{measure};
            $next_set = $param{set};
            $pattern  = $param{pattern};
            [qw/1 0/], 1;
        }
    }
);

ok( $set->voices->@* == 2 );

my $id = 0;
for my $v ( $set->voices->@* ) {
    is( $v->id, $id++ );
}

# ->advance can croak in RhythmSet.pm and also in Voice.pm
lives_ok { $set->advance( 1, foo => 'bar' ) };

is( $foo, 'bar' );
ok( defined $next_set and refaddr($set) eq refaddr($next_set) );
$deeply->( $pattern, [qw/1 0/] );

lives_ok { $set->advance };
is( $measure, 1 );    # first measure is 0

for my $v ( $set->voices->@* ) {
    is( $v->measure, 2 );
}

$set->measure(42);
for my $v ( $set->voices->@* ) {
    is( $v->measure, 42 );
}

$deeply->(
    $set->to_ly( maxm => 1 ),
    [ "  % v0 .x 1\n  r16 c16\n", "  % v1 x. 1\n  c16 r16\n" ],
);
$deeply->(
    $set->to_ly(
        maxm  => 1,
        note  => 'd',
        rest  => 's',
        voice => [ {}, { note => 'b' } ]
    ),
    [ "  % v0 .x 1\n  s16 d16\n", "  % v1 x. 1\n  b16 s16\n" ],
);

my $opus;
lives_ok { $opus = $set->to_midi( maxm => 1 ) };
ok( defined $opus and $opus->tracks == 2 );

lives_ok {
    $opus = $set->to_midi(
        maxm   => 1,
        format => 0,
        ticks  => 89,
        chan   => 3,
        track  => [ {}, { chan => 4 } ],
    )
};
ok( defined $opus and $opus->tracks == 2 );
# however, you probably do not want format 0 with multiple tracks; the
# MIDI players I have do not like that
is( $opus->format, 0 );
is( $opus->ticks,  89 );
my @tracks = $opus->tracks;
my @chan;
for my $t ( 0 .. 1 ) {
    for my $e ( $tracks[$t]->events_r->@* ) {
        if ( $e->[0] eq 'note_on' ) {
            $chan[$t] = $e->[2];
            last;
        }
    }
}
is( $chan[0], 3 );
is( $chan[1], 4 );

# ->to_string, ->from_string
{
    my $str = $set->to_string( maxm => 1 );
    is( $str, "0\t0\t.x\t1\n0\t1\tx.\t1\n" );

    my $ns = Music::RhythmSet->new->from_string($str);
    is( scalar $ns->voices->@*, 2 );
    # from_string only populates the replay log
    $deeply->( $ns->voices->[0]->replay, [ [ [ 0, 1 ], 1 ] ] );
    $deeply->( $ns->voices->[1]->replay, [ [ [ 1, 0 ], 1 ] ] );

    $str = "\n\n   \n" . $str;
    lives_ok { $ns = Music::RhythmSet->new->from_string($str) };

    $str = $set->to_string( maxm => 1, sep => ' ', rs => "\r" );
    is( $str, "0 0 .x 1\r0 1 x. 1\r" );
    $ns = Music::RhythmSet->new->from_string( $str, sep => ' ', rs => "\r" );
    is( scalar $ns->voices->@*, 2 );

    $str .= "\r   # EOS\r";
    lives_ok {
        $ns = Music::RhythmSet->new->from_string( $str, sep => ' ', rs => "\r" )
    };

    dies_ok { $ns->from_string } qr/need a string/;
    dies_ok { $ns->from_string('') } qr/need a string/;
    dies_ok { $ns->from_string('x') } qr/invalid record/;
    dies_ok { $ns->from_string("0\t\tx..\t42") } qr/invalid record/;
    dies_ok { $ns->from_string("0\tbadid\tx..\t42") } qr/invalid record/;
    dies_ok { $ns->from_string("0\t99\tx..\t42") } qr/ID out of range/;
    dies_ok { $ns->from_string("0\t0\t\t42") } qr/invalid record/;
    dies_ok { $ns->from_string("0\t0\tinvalid\t42") } qr/invalid record/;
    dies_ok { $ns->from_string("0\t0\tx..\tbadttl") } qr/invalid record/;
}

# ->clone
my $newset;
lives_ok { $newset = $set->clone };
ok( $newset->voices->@* == 2 );
ok( refaddr( $set->voices->[$_] ) ne refaddr( $newset->voices->[$_] ) )
  for 0 .. 1;
$deeply->( $newset->voices->[0]->pattern, [qw/0 1/] );
$deeply->( $newset->voices->[1]->pattern, [qw/1 0/] );

dies_ok { $set->add } qr/nothing to add/;
dies_ok { $set->add(undef) } qr/nothing to add/;
dies_ok { $set->add( [] ) } qr/invalid voice parameters/;

# ->changes, two-beat measures in two voices
my @measures;
my @args;
$set = Music::RhythmSet->new->add(
    {   next => sub { [qw/1 1/], 3 }
    },
    {   next => sub {
            state $i = 0;
            [ split '', sprintf "%02b", $i++ ], 2;
        }
    },
)->advance(6)->changes(
    divisor => 2,
    max     => 4,
    header  => sub { push @measures, $_[0] },
    voice   => sub {
        my (@rest) = @_;
        my $id     = splice @rest, 1, 1;
        push $args[$id]->@*, [@rest];
    },
);

$deeply->( \@measures, [ 0, 2, 3 ] );
$deeply->(
    \@args,
    # voice 0 -- ttl 3, always same pattern
    [   [   [ 0, [ 1, 1 ], "xx", 1,     undef ],    # changed
            [ 2, [ 1, 1 ], "xx", undef, undef ],    # -- other voice
            [ 3, [ 1, 1 ], "xx", 1,     1 ],        # changed repeat
        ],
        # voice 1 -- ttl 2, pattern counts binary
        [   [ 0, [ 0, 0 ], "..", 1,     undef ],    # changed
            [ 2, [ 0, 1 ], ".x", 1,     undef ],    # changed
            [ 3, [ 0, 1 ], ".x", undef, undef ],    # -- other voice
        ],
    ]
);

@measures = @args = ();

# without a divisor (the default) the "measure" is a single beat
$set = Music::RhythmSet->new->add(
    {   next => sub {
            state $i = 0;
            $i ^= 1;
            [$i], 1;
        }
    },
)->advance(3)->changes(
    header => sub { push @measures, $_[0] },
    # NOTE \@_ may yeild wacky results, instead make a copy of what's on
    # that stack
    voice => sub { push @args, [@_] },
);
$deeply->( \@measures, [ 0, 1, 2 ] );
$deeply->(
    \@args,
    [   [ 0, 0, [1], "x", 1, undef ],
        [ 1, 0, [0], ".", 1, undef ],
        [ 2, 0, [1], "x", 1, undef ],
    ]
);

# need the two mandatory callbacks
dies_ok { $set->changes };
dies_ok {
    $set->changes( header => sub { "foo" } )
};
dies_ok {
    $set->changes( voice => {}, header => sub { "foo" } )
};
dies_ok {
    $set->changes( voice => sub { "foo" }, header => {} )
};

# voicel, sugar for ->new->add(...)
$set = Music::RhythmSet->new( voicel => [ { pattern => [ 1, 0, 1 ] } ] );
$deeply->( $set->voices->[0]->pattern, [ 1, 0, 1 ] );
dies_ok { Music::RhythmSet->new( voicel => undef ) };
dies_ok { Music::RhythmSet->new( voicel => {} ) };
