# -*- Perl -*-
#
# various functions related to the generation and comparison of patterns
# of beats. a pattern of beats is assumed to be an array reference of
# zeros and ones

package Music::RhythmSet::Util;
our $VERSION = '0.01';

use 5.24.0;
use warnings;
use Carp qw(croak);
use Statistics::Lite qw(stddevp);

use constant { NOTE_ON => 1, NOTE_OFF => 0 };

use parent qw(Exporter);
our @EXPORT_OK =
  qw(compare_onsets filter_pattern rand_onsets score_fourfour score_stddev);

sub compare_onsets {
    my ($first, $second) = @_;
    my $same   = 0;
    my $onsets = 0;
    for my $i (0 .. $first->$#*) {
        if ($first->[$i] == NOTE_ON) {
            $onsets++;
            $same++ if $second->[$i] == NOTE_ON;
        }
    }
    croak "no onsets?! [@$first] [@$second]" unless $onsets;
    return $same / $onsets;
}

sub filter_pattern {
    my ($onsets, $total, $trials, $fudge, $nozero) = @_;
    $fudge //= 0.0039;
    my $best = ~0;
    my $pattern;
    for (1 .. $trials) {
        my $new   = &rand_onsets;
        my $score = score_stddev($new) + score_fourfour($new) * $fudge;
        next if $nozero and $score == 0;
        if ($score < $best) {
            $best    = $score;
            $pattern = $new;
        }
    }
    return $pattern;
}

sub rand_onsets {
    my ($onsets, $total) = @_;
    croak "onsets must be < total" if $onsets >= $total;
    my @pattern;
    while ($total) {
        if (rand() < $onsets / $total) {
            push @pattern, NOTE_ON;
            $onsets--;
        } else {
            push @pattern, NOTE_OFF;
        }
        $total--;
    }
    return \@pattern;
}

sub score_fourfour {
    my $beats       = shift;
    my @beatquality = map { 256 - $_ } qw(
      256 0 16 4
      64 0 32 8
      128 0 16 4
      64 0 32 8
    );
    my $score = 0;
    my $i     = 0;

    for my $x ($beats->@*) {
        $score += $beatquality[$i] if $x == NOTE_ON;
        $i++;
    }
    return $score;
}

sub score_stddev {
    my $beats = shift;
    my $len   = $beats->@*;
    my $sum   = 0;
    my @deltas;
    for my $i (0 .. $beats->$#*) {
        if ($beats->[$i] == 1) {
            my $j = $i + 1;
            while (1) {
                if ($beats->[ $j % $len ] == NOTE_ON) {
                    my $d = $j - $i;
                    push @deltas, $d;
                    $sum += $d;
                    last;
                }
                $j++;
            }
        }
    }
    croak "no onsets?! [@$beats]" unless @deltas;
    return stddevp(@deltas);
}

1;
__END__

=head1 NAME

Music::RhythmSet::Util - pattern generation and classification functions

=head1 DESCRIPTION

Various functions related to the generation and comparison of patterns
of beats. A I<pattern> of beats is assumed to be an array reference of
zeros and ones, e.g. for 4/4 time in 16th notes

  [ 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0 ]

Nothing is exported by default; functions will need to be used fully
qualified or be imported by listing them on the C<use> line.

=head1 FUNCTIONS

=over 4

=item B<compare_onsets> I<pattern1> I<pattern2>

What percentage of onsets between two patterns are identical? Makes
assumptions about the patterns which may not be true; ideally feed it
patterns of the same length.

  compare_onsets([1,0,0,0],[1,0,1,0])

=item B<filter_pattern> I<onsets> I<total> I<trials> ...

Generates I<trials> number of patterns via B<rand_onsets> and selects
for the "best" pattern by the lowest combined score of B<score_stddev>
and B<score_fourfour>. This routine will need to be profiled and tuned
for the need at hand; see the C<eg/variance> script under this module's
distribution for one way to study how the function behaves.

=item B<rand_onsets> I<onsets> I<total>

Randomly turns on I<onsets> in I<total> beats and returns that as an
array reference of zeros and ones. Will likely need to be filtered
somehow to select for more usable results.

=item B<score_fourfour> I<pattern>

Fiddled with by hand so that a lower score is something closer to one
opinion of 4/4 time in 16th notes. A (probably poor) attempt to select
for patterns such as

  [ 1,0,0,0, 0,0,1,0, 1,0,0,0, 0,0,1,0 ]

and not the identical but rotated off-beat

  [ 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,1,0,1 ]

Assumes the pattern is 16 beats in length.

=item B<score_stddev> I<pattern>

Standard deviation of the distances to the following onset; lower scores
indicate higher regularity (non-clumping) of the onsets. But you
probably want a rhythm somewhere between the zero score

  [ 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0 ]

and

  [ 1,1,1,1, 0,0,0,0, 0,0,0,0, 0,0,0,0 ]

(or the various rotations of the above) as the first is probably too
regular and the second probably too irregular.

This method should work on patterns of any length (CPU and memory
permitting).

=back

=head1 BUGS

<https://github.com/thrig/Music-RhythmSet>

=head1 SEE ALSO

L<Music::AtonalUtil> has various relevant routines, especially for beat
patterns of length 12.

"The Geometry of Musical Rhythm" by Godfried T. Toussaint.

=head1 COPYRIGHT AND LICENSE

Copyright 2021 Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<https://opensource.org/licenses/BSD-3-Clause>

=cut
