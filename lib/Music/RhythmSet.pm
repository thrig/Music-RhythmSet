# -*- Perl -*-
#
# sets of rhythms, comprised of one or more voices (or tracks) and
# various utility functions

package Music::RhythmSet;
our $VERSION = '0.01';

use 5.24.0;
use warnings;
use Carp qw(croak);
use MIDI;
use Moo;
use namespace::clean;
use List::GroupingPriorityQueue qw(grpriq_add);
use Music::RhythmSet::Voice;

has curid  => (is => 'rw', default => sub { 0 });
has voices => (is => 'rw', default => sub { [] });

# the 'id' attribute tries to match the array index in 'voices' though
# may not if the caller manually fiddles with either the voices list
# or the ID values (in which case they get what they deserve when
# things break)
sub add {
    my ($self, @rest) = @_;
    croak "nothing to add" unless @rest;
    my $id = $self->curid;
    for my $ref (@rest) {
        croak "invalid voice parameters"
          unless defined $ref and ref $ref eq 'HASH';
        $ref->{id} = $id++;
        push $self->voices->@*, Music::RhythmSet::Voice->new($ref->%*);
    }
    $self->curid($id);
    return $self;
}

sub advance {
    my ($self, $count, %param) = @_;
    # this is done stepwise for each voice so that TTL expirations and
    # thus potential new patterns are more likely to be visible to other
    # voices. voices that depend on other voices should therefore be
    # added after those other voices (or there could be a two- or N-pass
    # system to resolve any inter-voice pattern generation difficulties,
    # but that's not supported here)
    for (1 .. $count // 1) {
        for my $v ($self->voices->@*) {
            $param{set} = $self;
            $v->advance(1, %param);
        }
    }
    return $self;
}

sub changes {
    my ($self, %param) = @_;
    for my $cb (qw{header voice}) {
        croak "need $cb callback"
          unless defined $param{$cb}
          and ref $param{$cb} eq 'CODE';
    }
    # patterns can be of different lengths between voices (or can vary
    # over time inside a voice), though may be the same in which case
    # the caller can divide the beat count by however many beats there
    # are in a measure to obtain the measure number. otherwise, the
    # "measure" is the number of beats since the start of the replay log
    $param{divisor} //= 1;
    $param{max}     //= ~0;
    my $queue = [];
    for my $voice ($self->voices->@*) {
        my $beat = 0;
        for my $ref ($voice->replay->@*) {
            my ($bp, $ttl) = $ref->@*;
            # build a priority queue of when voices change their pattern
            grpriq_add($queue, [ $voice->id, $bp ], $beat);
            $beat += $ttl * $bp->@*;
        }
    }
    my @current;
    # parse the queue for pattern changes and let the caller decide
    # how to act on the results (see eg/beatinator for one way)
    for my $entry ($queue->@*) {    # [[id,[bp]],...],priority
        my $measure = $entry->[1] / $param{divisor};
        last if $measure > $param{max};
        my @changed;
        for my $ref ($entry->[0]->@*) {
            my ($id, $bp) = $ref->@*;
            $changed[$id] = 1;
            $current[$id] = $bp;
        }
        $param{header}->($measure);
        for my $id (0 .. $#current) {
            $param{voice}->($id, $current[$id], $changed[$id]);
        }
    }
}

sub clone {
    my ($self) = @_;
    my $new = Music::RhythmSet->new(curid => scalar $self->curid);
    my @voices;
    for my $v ($self->voices->@*) {
        push @voices, $v->clone;
    }
    $new->voices(\@voices);
    return $new;
}

sub measure {
    my ($self, $maxm) = @_;
    for my $v ($self->voices->@*) {
        $v->measure($maxm);
    }
}

sub to_ly {
    my ($self, $maxm, %param) = @_;
    croak "need measure count" unless defined $maxm and $maxm >= 1;
    for my $i (0 .. $self->voices->$#*) {
        for my $p (qw/dur note rest/) {
            $param{voice}[$i]{$p} = $param{$p}
              if exists $param{$p} and not exists $param{voice}[$i]{$p};
        }
    }
    my $i = 0;
    return [ map { $_->to_ly($maxm, $param{voice}->[ $i++ ]->%*) }
          $self->voices->@* ];
}

sub to_midi {
    my ($self, $maxm, %param) = @_;
    croak "need measure count" unless defined $maxm and $maxm >= 1;
    $param{format} //= 1;
    $param{ticks}  //= 96;
    for my $i (0 .. $self->voices->$#*) {
        for my $p (qw/chan dur note notext tempo sustain velo/) {
            $param{track}[$i]{$p} = $param{$p}
              if exists $param{$p} and not exists $param{track}[$i]{$p};
        }
    }
    my $i = 0;
    return MIDI::Opus->new(
        {   format => $param{format},
            ticks  => $param{ticks},
            tracks => [
                map { $_->to_midi($maxm, $param{track}->[ $i++ ]->%*) }
                  $self->voices->@*
            ]
        }
    );
}

1;
__END__

=head1 NAME

Music::RhythmSet - sets of rhythms and various generation functions

=head1 SYNOPSIS

  use 5.24.0;
  use Music::RhythmSet;
  use Music::RhythmSet::Util qw(rand_onsets);
  
  my $rest = [ (0) x 16 ];
  
  # randomly select a rhythm with five onsets in 16 beats
  # that will live for eight measures
  sub newpat { rand_onsets(5, 16), 8 }
  
  # three voices, with a delayed entrance on two of them
  my $set = Music::RhythmSet->new->add(
      { pattern => rand_onsets(5, 16), ttl => 16 },
      { next => \&newpat, pattern => $rest, ttl => 2 },
      { next => \&newpat, pattern => $rest, ttl => 4 },
  );
  
  # generate 8 measures of (probably) noise
  $set->advance(8);
  
  # export with different notes for each track
  $set->to_midi( 8,
      track => [ {}, { note => 67 }, { note => 72 } ]
  )->write_to_file("noise.midi");

=head1 DESCRIPTION

This module supports sets of rhythms, each being a
L<Music::RhythmSet::Voice> object, which is where most of the action
happens. L<Music::RhythmSet::Util> offers various rhythm generation and
scoring functions. Rhythms are given a lifetime, and a callback function
can set a new rhythm and time-to-live for it as a rhythmic line is
generated. Rhythms can be exported in various formats.

Various calls may B<die> or B<croak> if something goes awry.

=head1 CONSTRUCTOR

The B<new> method accepts any of the L</ATTRIBUTES>, but these are
unlikely to need to be set manually.

=head1 ATTRIBUTES

=over 4

=item B<curid>

Internal variable. ID (or array index) for the next voice to be added
with the B<add> method. Should not be changed unless you are manually
editing the B<voices> list.

=item B<voices>

Array reference of voices. These are L<Music::RhythmSet::Voice> objects.
Probably should not be manually edited.

=back

=head1 METHODS

=over 4

=item B<add> I<voice> [ I<voice> ... ]

Pushes the I<voices> onto the B<voices> attribute, and sets the B<id>
parameter of each voice to (hopefully) what is the array index of that
voice in the I<voices> list. Each voice is a hash reference that is fed
to the constructor of L<Music::RhythmSet::Voice>. However, a
caller-supplied I<id> attribute will ignored as this module manages
those values itself.

=item B<advance> I<count> I<param>

This call steps each of the voices forward by I<count> measures, which
may result in new entries into the replay log for each voice, as well as
callbacks being run to set new rhythms. Voices are advanced in turn from
first to last in the voices list.

I<param> is used to pass data down to the B<advance> method of
L<Music::RhythmSet::Voice> and from there into the B<next> callback. In
particular the I<set> attribute will contain a reference to the C<$set>
object involved, in the event voices need to query other voices during a
B<next> callback.

=item B<changes> I<param>

Utility method that shows when voices change their patterns in their
replay logs, and what other patterns are active at those points. Voices
should have something in their replay log before this method is called.
The C<eg/beatinator> script in this module's distribution shows one way
to use this call.

Note that changes to the same pattern as previously set are considered
a change; this shows when the B<next> method (or similar) is doing
something to a pattern, even if that is setting it to the same value
as before.

There are two mandatory parameters:

=over 4

=item I<header>

Callback; it is passed the current "measure" number of the change.

=item I<voice>

Callback; it is passed the voice ID, the current pattern for that voice
ID, and a boolean that indicates whether or not the pattern is new for
this "measure".

=back

And two optional parameters:

=over 4

=item I<divisor>

A positive integer that indicates how many beats there are in a measure.
C<1> by default, which means a "measure" is the number of beats since
the beginning of the replay log (counting from 0, not 1). A divisor of
C<16> (and assuming the I<pattern> used are all of length 16) would mean
that the term "measure" no longer needs scare quotes, as it would
represent a measure number as they are more typically known (except for
the counting from zero thing, which musicians usually do not do).

=item I<max>

A positive integer for when to stop working through the "measures" of
the replay log.

=back

=item B<clone>

Clones each of the voices and returns a new L<Music::RhythmSet> object
with those cloned voices.

=item B<measure> I<count>

Sets the B<measure> attribute of each voice to the given I<count>.
Possibly useful when reloading from a replay file or under similar
manual edits to the voices so that any subsequent B<advance> calls use
the correct measure number in any relevant B<next> callback
calculations.

Voices that use measures (patterns) of different sizes will need their
measure count set individually.

=item B<to_ly> I<measure-count> I<param>

Returns an array reference of strings that contain the replay log
formated for lilypond.

  my $i = 0;
  for my $s ($set->to_ly(32)->@*) {
      write_text("noise.$i.ly", $s);
      $i++;
  }

The files can then be included from another lilypond file:

  \version "2.18.2"
  lino = \relative c' { \include "noise.0.ly" }
  lipa = \relative c' { \include "noise.1.ly" }
  lire = \relative c' { \include "noise.2.ly" }
  lezgike = { <<
    \new Staff << \lino >>
    \new Staff << \lipa >>
    \new Staff << \lire >>
  >> }
  \score { \lezgike \layout { } \midi { } }

The I<param> can include a I<voice> element; this allows the I<dur>,
I<note>, and I<rest> parameters of the individual voices to be
customized. I<dur>, I<note>, and I<rest> can also be set at the top
level to change the defaults for all the voices, unless there is a more
specific setting for a voice.

  my $ret = $set->to_ly( 32,
      # quarter notes for all voices
      dur => 4,
      # voice specifics
      voice => [
          { note => 'b' },
          { note => 'a', rest => 's' },
          { note => 'c' }
      ]
  );

=item B<to_midi> I<measure-count> I<param>

Returns a I<MIDI::Opus> object containing tracks for each of the voices.
Will likely need to be saved with the B<write_to_file> method call.
Parameters accepted include I<format> (probably should be C<1>),
I<ticks>, and I<track>. I<track> allows parameters for the
L<Music::RhythmSet::Voice> B<to_midi> call to be passed to a specific
voice. These can also be specified in this B<to_midi> call to apply to
all the tracks:

  my $opus = $set->to_midi( 16,
      tempo => 640_000,
      track => [ {}, { note => 67 }, { note => 72 } ]
  );

L<MIDI::Event> documents most of the values the I<track>
parameters can take.

=back

=head1 BUGS

<https://github.com/thrig/Music-RhythmSet>

=head1 SEE ALSO

L<MIDI>, L<Music::AtonalUtil>, L<Music::RecRhythm>

=head1 COPYRIGHT AND LICENSE

Copyright 2021 Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<https://opensource.org/licenses/BSD-3-Clause>

=cut
