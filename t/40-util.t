#!perl
#
# these utility routines are pretty light on error checking

use 5.24.0;
use Test::Most tests => 21;
my $deeply = \&eq_or_diff;

use Music::RhythmSet::Util
  qw(compare_onsets filter_pattern rand_onsets score_fourfour score_stddev);

is(sprintf('%.0f', compare_onsets([qw/1 1/], [qw/1 1/])), 1);
is(sprintf('%.1f', compare_onsets([qw/1 1/], [qw/1 0/])), 0.5);
dies_ok { compare_onsets([], []) };

# odds are, anyways
$deeply->(
    filter_pattern(4, 16, 10000),
    [ 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ]
);

my ($on, @slots, $total);
for (1 .. 100) {
    my $pat = rand_onsets(5, 10);
    for my $i (0 .. $pat->$#*) {
        if ($pat->[$i] == 1) {
            $on++;
            $slots[$i]++;
        }
        $total++;
    }
}
# exactly half of the onsets should be turned on
my $half = int($total / 2);
is($on, $half);
# but where the onsets are will hopefully vary and will hopefully be
# evenly divided between the slots. this might also be testing the RNG
my $tolerance = 0.05;
for my $s (@slots) {
    my $vary = abs($s / $half - 1 / @slots);
    ok($vary < $tolerance);
}
dies_ok { rand_onsets(1, 1) };

is(score_fourfour([]),                                    0);
is(score_fourfour([qw/1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0/]), 512);

is(score_stddev([qw/1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0/]), 0);
is(score_stddev([qw/0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0/]), 0);
# TODO actually check the math on this one but that would probably be
# testing whatever Statistics::Lite is doing
is( sprintf("%.1f",
        score_stddev([qw/1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0/])),
    5.2
);
