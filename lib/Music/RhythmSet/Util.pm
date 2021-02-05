# -*- Perl -*-
#
# various functions related to the generation and comparison of patterns
# of beats, and etc

package Music::RhythmSet::Util;
our $VERSION = '0.01';

use 5.24.0;
use warnings;
use Carp qw(croak);
use Statistics::Lite qw(stddevp);

use constant { NOTE_ON => 1, NOTE_OFF => 0 };

use parent qw(Exporter);
our @EXPORT_OK =
  qw(beatstring compare_onsets duration filter_pattern flatten ocvec onset_count rand_onsets score_fourfour score_stddev write_midi);

sub beatstring {
    my ($bpat) = @_;
    croak "no pattern set"
      unless defined $bpat and ref $bpat eq 'ARRAY';
    return join('', $bpat->@*) =~ tr/10/x./r;
}

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

sub duration {
    my ($replay) = @_;
    croak "no replay log"
      unless defined $replay and ref $replay eq 'ARRAY';
    my $measures = 0;
    my $beats    = 0;
    for my $ref ($replay->@*) {
        $measures += $ref->[1];
        $beats    += $ref->[0]->@* * $ref->[1];
    }
    return $measures, $beats;
}

sub filter_pattern {
    my ($onsets, $total, $trials, $fudge, $nozero) = @_;
    $fudge //= 0.0039;
    my $best = ~0;
    my $bpat;
    for (1 .. $trials) {
        my $new   = &rand_onsets;
        my $score = score_stddev($new) + score_fourfour($new) * $fudge;
        next if $nozero and $score == 0;
        if ($score < $best) {
            $best = $score;
            $bpat = $new;
        }
    }
    return $bpat;
}

sub flatten {
    my ($replay) = @_;
    croak "no replay log"
      unless defined $replay and ref $replay eq 'ARRAY';
    return [ map { ($_->[0]->@*) x $_->[1] } $replay->@* ];
}

# "onset-coordinate vector" notation for a pattern
sub ocvec {
    my ($bpat) = @_;
    croak "no pattern set"
      unless defined $bpat and ref $bpat eq 'ARRAY';
    my @set;
    my $i = 0;
    for my $x ($bpat->@*) {
        push @set, $i if $x == NOTE_ON;
        $i++;
    }
    return \@set;
}

sub onset_count {
    my ($bpat) = @_;
    croak "no pattern set"
      unless defined $bpat and ref $bpat eq 'ARRAY';
    my $onsets = 0;
    for my $x ($bpat->@*) {
        $onsets++ if $x == NOTE_ON;
    }
    return $onsets;
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
    my @deltas;
    for my $i (0 .. $beats->$#*) {
        if ($beats->[$i] == NOTE_ON) {
            my $j = $i + 1;
            while (1) {
                if ($beats->[ $j % $len ] == NOTE_ON) {
                    my $d = $j - $i;
                    push @deltas, $d;
                    last;
                }
                $j++;
            }
        }
    }
    croak "no onsets?! [@$beats]" unless @deltas;
    return stddevp(@deltas);
}

sub write_midi {
    my ($file, $track, %param) = @_;
    $param{format} //= 1;
    $param{ticks}  //= 96;
    MIDI::Opus->new(
        {   format => $param{format},
            ticks  => $param{ticks},
            tracks => ref $track eq 'ARRAY' ? $track : [$track]
        }
    )->write_to_file($file);
    return;
}

1;
__END__

=head1 NAME

Music::RhythmSet::Util - pattern generation and classification functions

=head1 DESCRIPTION

Various functions related to the generation and comparison of patterns
of beats, and so forth. A I<pattern> of beats is assumed to be an array
reference of zeros and ones, e.g. for 4/4 time in 16th notes

  [ 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0 ]

Nothing is exported by default; functions will need to be used fully
qualified or be imported by listing them on the C<use> line.

=head1 FUNCTIONS

=over 4

=item B<beatstring> I<pattern>

Converts a I<pattern> such as C<[qw/1 0 1 0/]> to a string such
as C<x.x.>.

=item B<compare_onsets> I<pattern1> I<pattern2>

What percentage of onsets between two patterns are identical? Makes
assumptions about the patterns which may not be true; ideally feed it
patterns of the same length.

  compare_onsets([1,0,0,0],[1,0,1,0])

=item B<duration> I<replay-log>

Returns a list consisting of the number of measures and the total number
of beats in those measures given a I<replay-log>.

=item B<filter_pattern> I<onsets> I<total> I<trials> ...

Generates I<trials> number of patterns via B<rand_onsets> and selects
for the "best" pattern by the lowest combined score of B<score_stddev>
and B<score_fourfour>. This routine will need to be profiled and tuned
for the need at hand; see the C<eg/variance> script under this module's
distribution for one way to study how the function behaves.

=item B<flatten> I<replay-log>

Flattens the given I<replay-log> into a single array reference of beats.

=item B<ocvec> I<pattern>

Converts a I<pattern> into "onset-coordinate vector" notation. This
format is suitable to be fed to L<Music::AtonalUtil>.

=item B<onset_count> I<pattern>

Returns a count of how many onsets there are in the I<pattern>.

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

=item B<write_midi> I<filename> I<track> [ I<params> ]

A small wrapper around L<MIDI::Opus> that writes a MIDI track (or
tracks) to a file. The optional I<params> may include I<format>
(probably should be C<1>) and I<ticks> (consult the MIDI specification).

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
