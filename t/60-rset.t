#!perl

use 5.24.0;
use Test::Most tests => 53;
my $deeply = \&eq_or_diff;

use Music::RhythmSet;
use Scalar::Util qw(refaddr);

my $set = Music::RhythmSet->new;

$set->stash("foo");
is($set->stash, "foo");

is($set->curid, 0);
$deeply->($set->voices, []);

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
            my ($self, %param) = @_;
            $foo      = $param{foo};
            $measure  = $param{measure};
            $next_set = $param{set};
            $pattern  = $param{pattern};
            [qw/1 0/], 1;
        }
    }
);

is($set->curid, 2);
ok($set->voices->@* == 2);

my $id = 0;
for my $v ($set->voices->@*) {
    is($v->id, $id++);
}

# ->advance can croak in RhythmSet.pm and also in Voice.pm
lives_ok { $set->advance(1, foo => 'bar') };

is($foo, 'bar');
ok(defined $next_set and refaddr($set) eq refaddr($next_set));
$deeply->($pattern, [qw/1 0/]);

lives_ok { $set->advance };
is($measure, 1);    # first measure is 0

for my $v ($set->voices->@*) {
    is($v->measure, 2);
}

$set->measure(42);
for my $v ($set->voices->@*) {
    is($v->measure, 42);
}

$deeply->(
    $set->to_ly(1),
    [ "  % v0 .x 1\n  r16 c16\n", "  % v1 x. 1\n  c16 r16\n" ],
);
$deeply->(
    $set->to_ly(
        1,
        note  => 'd',
        rest  => 's',
        voice => [ {}, { note => 'b' } ]
    ),
    [ "  % v0 .x 1\n  s16 d16\n", "  % v1 x. 1\n  b16 s16\n" ],
);

my $opus;
lives_ok { $opus = $set->to_midi(1) };
ok(defined $opus and $opus->tracks == 2);

lives_ok {
    $opus = $set->to_midi(
        1,
        format => 0,
        ticks  => 89,
        chan   => 3,
        track  => [ {}, { chan => 4 } ],
    )
};
ok(defined $opus and $opus->tracks == 2);
# however, you probably do not want format 0 with multiple tracks; the
# MIDI players I have do not like that
is($opus->format, 0);
is($opus->ticks,  89);
my @tracks = $opus->tracks;
my @chan;
for my $t (0 .. 1) {
    for my $e ($tracks[$t]->events_r->@*) {
        if ($e->[0] eq 'note_on') {
            $chan[$t] = $e->[2];
            last;
        }
    }
}
is($chan[0], 3);
is($chan[1], 4);

# ->clone
my $newset;
lives_ok { $newset = $set->clone };
is($newset->curid, 2);
ok($newset->voices->@* == 2);
ok( refaddr($set->voices->[$_]) ne refaddr($newset->voices->[$_]))
  for 0 .. 1;
$deeply->($newset->voices->[0]->pattern, [qw/0 1/]);
$deeply->($newset->voices->[1]->pattern, [qw/1 0/]);

dies_ok { $set->add };
dies_ok { $set->add(undef) };
dies_ok { $set->add([]) };

is($set->curid, 2);

dies_ok { $set->to_ly };
dies_ok { $set->to_ly(0) };
dies_ok { $set->to_midi };
dies_ok { $set->to_midi(0) };

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

$deeply->(\@measures, [ 0, 2, 3 ]);
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
$deeply->(\@measures, [ 0, 1, 2 ]);
$deeply->(
    \@args,
    [   [ 0, 0, [1], "x", 1, undef ],
        [ 1, 0, [0], ".", 1, undef ],
        [ 2, 0, [1], "x", 1, undef ],
    ]
);

dies_ok { $set->changes };
dies_ok {
    $set->changes(header => sub { "foo" })
};
dies_ok {
    $set->changes(voice => {}, header => sub { "foo" })
};
dies_ok {
    $set->changes(voice => sub { "foo" }, header => {})
};

# voicel, sugar for ->new->add(...)
$set = Music::RhythmSet->new(
    voicel => [ { pattern => [ 1, 0, 1 ] } ]);
$deeply->($set->voices->[0]->pattern, [ 1, 0, 1 ]);
dies_ok { Music::RhythmSet->new(voicel => undef) };
dies_ok { Music::RhythmSet->new(voicel => {}) };
