#!perl

use 5.24.0;
use Test::Most tests => 49;
my $deeply = \&eq_or_diff;

use Music::RhythmSet;
use Scalar::Util qw(refaddr);

my $set = Music::RhythmSet->new;

is($set->curid, 0);
$deeply->($set->voices, []);

# to capture results from the next callback
my $foo;
my $measure = 0;
my $next_set;
my $pattern = [];

$set->add(
    { pattern => [qw/0 1/], ttl => 2, id => 42 },
    {   pattern => [qw/1 0/],
        ttl     => 1,
        next    => sub {
            my ($self, %param) = @_;
            $foo      = $param{foo};
            $measure  = $param{measure};
            $next_set = $param{set};
            $pattern  = $param{pattern};
            [qw/1 0/], 4;
        }
    }
);

is($set->curid, 2);
ok($set->voices->@* == 2);

my $id = 0;
for my $v ($set->voices->@*) {
    is($v->id, $id++);
}

$set->advance;
$set->advance(1, foo => 'bar');

is($foo,     'bar');
is($measure, 1);       # first measure is 0
ok(defined $next_set and refaddr($set) eq refaddr($next_set));
$deeply->($pattern, [qw/1 0/]);

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

# ->changes ... TODO better tests than this?
my $beat = 0;
my @changes;
lives_ok {
    $set->changes(
        header => sub { $beat = shift },
        voice  => sub { push @changes, \@_ },
    )
};
is($beat, 2);
$deeply->(
    \@changes,
    [   [ 0, [ 0, 1 ], 1 ],        # voice 0, pattern, is-new?
        [ 1, [ 1, 0 ], 1 ],        # voice 1
        [ 0, [ 0, 1 ], undef ],    # TTL 2 -> undef is-new?
        [ 1, [ 1, 0 ], 1 ]         # TTL 1 so this voice "changed"
    ]
);

# otherwise voice 0 would croak
$set->voices->[0]->next(sub { [ 1, 1 ], 3 });
$set->advance(10);

my $measures = 0;
lives_ok {
    $set->changes(
        divisor => 2,                     # two beats per measure
        max     => 2,
        header  => sub { $measures++ },
        voice   => sub { $pattern = $_[1] if $_[0] eq 0 },
    )
};
is($measures, 3);
$deeply->($pattern, [ 1, 1 ]);

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
