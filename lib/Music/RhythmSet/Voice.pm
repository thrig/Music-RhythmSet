# -*- Perl -*-
#
# a voice (or track) that is comprised of various patterns repeated ttl
# times and some utility functions for building out rhythms over time

package Music::RhythmSet::Voice;
our $VERSION = '0.01';

use 5.24.0;
use warnings;
use Carp qw(confess croak);
use MIDI;
use Moo;
use namespace::clean;

use constant { NOTE_ON => 1, NOTE_OFF => 0 };

use parent qw(Exporter);
our @EXPORT_OK = qw(duration flatten ocvec onset_count write_midi);

has id      => (is => 'rw');
has next    => (is => 'rw');
has measure => (is => 'rw', default => sub { 0 });
has pattern => (is => 'rw');
has replay  => (is => 'rw', default => sub { [] });
has ttl     => (is => 'rw', default => sub { 0 });

# perldoc Moo
sub BUILD {
    my ($self, $args) = @_;
    if (exists $args->{pattern} and exists $args->{ttl}) {
        croak "invalid ttl" if $args->{ttl} < 1;
        croak "invalid pattern"
          unless defined $args->{pattern}
          and ref $args->{pattern} eq 'ARRAY';
        push $self->replay->@*, [ $args->{pattern}, $args->{ttl} ];
    }
}

########################################################################
#
# FUNCTIONS

# TODO 'changes' but for a single voice... can easily enough put a voice
# into a set then call changes there, meanwhile

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

sub flatten {
    my ($replay) = @_;
    croak "no replay log"
      unless defined $replay and ref $replay eq 'ARRAY';
    return [ map { ($_->[0]->@*) x $_->[1] } $replay->@* ];
}

# "onset-coordinate vector" notation for a pattern
sub ocvec {
    my ($pat) = @_;
    croak "no pattern set"
      unless defined $pat and ref $pat eq 'ARRAY';
    my @set;
    my $i = 0;
    for my $v ($pat->@*) {
        push @set, $i if $v == NOTE_ON;
        $i++;
    }
    return \@set;
}

sub onset_count {
    my ($pat) = @_;
    croak "no pattern set"
      unless defined $pat and ref $pat eq 'ARRAY';
    my $onsets = 0;
    for my $v ($pat->@*) {
        $onsets++ if $v == NOTE_ON;
    }
    return $onsets;
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
}

########################################################################
#
# METHODS

sub advance {
    my ($self, $count, %param) = @_;
    my $measure = $self->measure;
    for (1 .. $count // 1) {
        my $ttl = $self->ttl - 1;
        $param{measure} = $measure++;
        $param{pattern} = $self->pattern;
        if ($ttl <= 0) {
            my $next = $self->next;
            confess "no next callback"
              unless defined $next and ref $next eq 'CODE';
            ($param{pattern}, $ttl) = $next->($self, %param);
            confess "no pattern set"
              unless defined $param{pattern}
              and ref $param{pattern} eq 'ARRAY'
              and $param{pattern}->@*;
            confess "invalid ttl" if $ttl < 1;
            $self->pattern($param{pattern});
            push $self->replay->@*, [ $param{pattern}, $ttl ];
        }
        $self->ttl($ttl);
    }
    $self->measure($measure);
    return $self;
}

sub clone {
    my ($self, $id) = @_;
    $id //= $self->id;
    my $new = Music::RhythmSet::Voice->new(
        id => $id,
        map { $_, scalar $self->$_ } qw(next measure ttl),
    );
    my $pat = $self->pattern;
    if (defined $pat) {
        die "invalid pattern" unless ref $pat eq 'ARRAY';
        $new->pattern([ $pat->@* ]);
    }
    my $ref = $self->replay;
    if (defined $ref) {
        die "replay must be an array reference"
          unless ref $ref eq 'ARRAY';
        die "replay array must contain array references"
          unless ref $ref->[0] eq 'ARRAY';
        $new->replay([ map { [ [ $_->[0]->@* ], $_->[1] ] } $ref->@* ]);
    }
    return $new;
}

# TODO some means of note reduction and optional note sustains
# over rests
sub to_ly {
    my ($self, $maxm, %param) = @_;
    croak "need measure count" unless defined $maxm and $maxm >= 1;
    my $replay = $self->replay;
    croak "empty replay log"
      unless defined $replay
      and ref $replay eq 'ARRAY'
      and $replay->@*;
    $param{dur}  //= '16';
    $param{note} //= 'c';
    $param{rest} //= 'r';
    my $id = $self->id // '';
    my $ly = '';

    for my $ref ($replay->@*) {
        my ($bp, $ttl) = $ref->@*;
        $ttl = $maxm if $ttl > $maxm;
        $ly .= "  % v$id " . join('', $bp->@*) =~
          tr/10/x./r . ' ' . $ttl . "\n";
        my $s = ' ';
        for my $v ($bp->@*) {
            if ($v == NOTE_ON) {
                $s .= ' ' . $param{note} . $param{dur};
            } else {
                $s .= ' ' . $param{rest} . $param{dur};
            }
        }
        $ly .= join("\n", ($s) x $ttl) . "\n";
        $maxm -= $ttl;
        last if $maxm <= 0;
    }
    return $ly;
}

sub to_midi {
    my ($self, $maxm, %param) = @_;
    croak "need measure count" unless defined $maxm and $maxm >= 1;
    my $replay = $self->replay;
    croak "empty replay log"
      unless defined $replay
      and ref $replay eq 'ARRAY'
      and $replay->@*;
    # MIDI::Event, section "EVENTS AND THEIR DATA TYPES"
    $param{chan}  //= 0;
    $param{dur}   //= 20;
    $param{note}  //= 60;
    $param{tempo} //= 500_000;
    $param{velo}  //= 96;
    my $track  = MIDI::Track->new;
    my $events = $track->events_r;

    my $id = $self->id // '';
    push $events->@*,
      [ 'track_name', 0, 'voice' . (length $id ? " $id" : '') ];
    push $events->@*, [ 'set_tempo', 0, $param{tempo} ];
    my $delay;
    my $leftover = 0;

    for my $ref ($replay->@*) {
        my ($bp, $ttl) = $ref->@*;
        $ttl = $maxm if $ttl > $maxm;
        push $events->@*,
          [ 'text_event', $leftover,
            "v$id " . join('', $bp->@*) =~ tr/10/x./r . " $ttl\n"
          ];
        $delay = 0;
        my ($onsets, $open, @midi);
        for my $v ($bp->@*) {
            if ($v == NOTE_ON) {
                $onsets++;
                if (defined $open) {
                    push @midi, [ 'note_off', $delay, $param{chan}, $open, 0 ];
                    $delay = 0;
                }
                push @midi,
                  [ 'note_on', $delay, map { $param{$_} } qw(chan note velo) ];
                $delay = $param{dur};
                $open  = $param{note};
            } else {
                if (defined $open) {
                    push @midi, [ 'note_off', $delay, $param{chan}, $open, 0 ];
                    $delay = 0;
                    undef $open;
                }
                $delay += $param{dur};
            }
        }
        if (defined $open) {
            push @midi, [ 'note_off', $delay, $param{chan}, $open, 0 ];
            $delay = 0;
        }
        # trailing rests create a delay that must be applied to the
        # start of subsequent repeats (if onsets make this possible) and
        # then passed on as leftovers for the next text_event
        if ($delay and $onsets and $ttl > 1) {
            push $events->@*, @midi;
            $midi[0] = [ $midi[0]->@* ];
            $midi[0][1] += $delay;
            push $events->@*, (@midi) x ($ttl - 1);
        } else {
            push $events->@*, (@midi) x $ttl;
        }
        # delay from trailing rests *or* a full measure of rest
        $leftover = $delay;
        # remainder of full measures of rest, if any
        $leftover += $bp->@* * $param{dur} * ($ttl - 1) unless $onsets;
        $maxm     -= $ttl;
        last if $maxm <= 0;
    }
    # mostly for sustain to have something to extend out to, and so that
    # different trailing rests between different voices do not result in
    # ragged track ends
    push $events->@*, [ 'text_event', $leftover, "v$id EOT\n" ];

    # and here the MIDI is modified if need be -- the above is already
    # complicated, and it's (somewhat) easier to cut events out and
    # fiddle with delays on the completed stream
    if ($param{sustain} or $param{notext}) {
        my $i = 0;
        while ($i < $events->$#*) {
            if ($param{sustain} and $events->[$i][0] eq 'note_off') {
                # extend delay on the note_off to the next note_on;
                # there might be a text_event between
                my $delay = 0;
                my $j     = $i + 1;
                while (1) {
                    if ($events->[$j][0] eq 'text_event' and $events->[$j][1] > 0) {
                        $delay += $events->[$j][1];
                        $events->[$j][1] = 0;
                    } elsif ($events->[$j][0] eq 'note_on') {
                        if ($events->[$j][1] > 0) {
                            $delay += $events->[$j][1];
                            $events->[$j] = [ $events->[$j]->@* ];
                            $events->[$j][1] = 0;
                        }
                        last;
                    }
                    last if ++$j > $events->$#*;
                }
                $events->[$i] = [ $events->[$i]->@* ];
                $events->[$i][1] += $delay;
            } elsif ($param{notext} and $events->[$i][0] eq 'text_event') {
                my $delay = $events->[$i][1];
                splice $events->@*, $i, 1;
                $events->[$i] = [ $events->[$i]->@* ];
                $events->[$i][1] += $delay;
                next;    # reprocess this index because splice
            }
            $i++;
        }
        # NOTE note_off cannot appear in the last index as that is
        # always filled by the EOT text_event. therefore sustain is not
        # checked for here and we assume that it is a text_event
        pop $events->@* if $param{notext};
    }

    return $track;
}

1;
__END__

=head1 NAME

Music::RhythmSet::Voice - a rhythmic line

=head1 SYNOPSIS

  use MIDI;
  use Math::Random::Discrete;
  use Music::RhythmSet::Voice qw(write_midi);
  
  # different selection odds for three patterns
  my $ppick = Math::Random::Discrete->new(
      [ 15,
        30,
        20, ],
      [ [qw/1 0 1 0 0 1 0 0/],
        [qw/1 0 1 0 0 0/],
        [qw/1 0 0 0 1 0/] ]);
  # and three ttl
  my $tpick = Math::Random::Discrete->new(
      [ 25, 45, 15, ],
      [ 1,  2,  4 ]);

  # pick a random pattern and ttl from above
  sub newpat { $ppick->rand, $tpick->rand }
  
  # zero ttl forces an immediate next call
  my $voice = Music::RhythmSet::Voice->new(
      id => 0, next => \&newpat, ttl => 0,
  );
  
  # generate 64 measures
  my $mm = 64;
  $voice->advance($mm);

  my $track = $voice->to_midi($mm, sustain => 1);
  write_midi('track.midi', $track);

=head1 DESCRIPTION

A single rhythmic voice (or track) with various routines to advance the
track over time and change the rhythm as necessary. L<Music::RhythmSet>
can store multiple voices, but most of the work is done by this module
for each voice.

Various calls may B<die> or B<croak> if something goes awry.

=head1 CONSTRUCTOR

The B<new> method accepts any of the L</ATTRIBUTES>. If both a
I<pattern> and a I<ttl> are given they will be added to the I<replay>
log. Another option is to only build out the I<replay> log via I<next>
callback through the B<advance> method, or to set the I<replay> log
manually. B<measure> may need to be set manually if the I<replay> log is
built not using B<advance> calls.

=head2 BUILD

Constructor helper subroutine. See L<Moo>.

=head1 ATTRIBUTES

=over 4

=item B<id>

An ID for the voice. This is set automatically by L<Music::RhythmSet>
but could be set manually.

=item B<next> I<code-reference>

A callback that runs when the I<ttl> expires. This routine must return
a new pattern and TTL. The callback is passed a reference to the
L<Music::RhythmSet::Voice> object, and a set of parameters with
various metadata. See L</CALLBACK> for more details.

If no callback function is set the B<advance> method may throw an error.

=item B<measure>

The current measure number of the voice. The first measure is C<0>, not
C<1>, though B<measure> will be C<1> following the first C<advance(1)>
call. The B<next> callback can make use of this to make decisions based
on the measure number, as B<measure> is passed in as a parameter:

  ... = Music::RhythmSet::Voice->new(
      next => sub {
          my ($self, %param) = @_;
          if ($param{measure} == 0) {   # first measure
              ...

The length of a measure will change should the length of the I<pattern>
used vary over time. This may complicate the display of the results or
other such calculations that rely on measure numbers, especially if
there are multiple voices that use different pattern lengths and vary
those lengths over time. See B<changes> in the code for
L<Music::RhythmSet> for one way to handle such a case.

=item B<pattern>

The current rhythmic pattern, an array reference of zeros and ones;
these might be called "beats" where a C<1> represents an onset, and C<0>
silence. A B<pattern> may be considered as a single measure of music (of
some number of beats which is the length of the B<pattern>), though
B<measure> is used for something else in this code.

=item B<replay>

An array reference of I<pattern> and I<ttl> pairs, usually created by
calling B<advance> for some number of measures with a suitable B<next>
callback set.

=item B<ttl>

Time-to-live of the current B<pattern>.

=back

=head1 FUNCTIONS

=over 4

=item B<duration> I<replay-log>

Returns a list consisting of the number of measures and the total number
of beats in those measures given a I<replay-log>.

=item B<flatten> I<replay-log>

Flattens the I<replay-log> into a single array reference of beats.

=item B<ocvec> I<pattern>

Converts a I<pattern> into "onset-coordinate vector" notation. This
format is suitable to be fed to L<Music::AtonalUtil>.

=item B<onset_count> I<pattern>

Returns a count of how many onsets there are in the I<pattern>.

=item B<write_midi> I<filename> I<track> [ I<params> ]

A small wrapper around L<MIDI::Opus> that writes the return value of
B<to_midi> to a file. The optional I<params> may include I<format>
(probably should be C<1>) and I<ticks> (consult the MIDI specification).

=back

=head1 METHODS

=over 4

=item B<advance> I<count> [ I<param> ]

Step the voice forward by I<count> measures. This may trigger the
B<next> attribute callback code and may result in new entries in the
replay log.

The B<to_*> methods will fail if there is nothing in the replay log;
this can happen when the replay log is generated only from B<next>
callback calls and you forget to call B<advance>.

=item B<clone> [ I<new-id> ]

Clones the object with a I<new-id> that if unset will be the same B<id>
of the current object, for better or worse.

=item B<to_ly> I<count> [ I<param> ]

Returns I<count> measures worth of the replay log formatted for lilypond
(a text string). Parameters include I<dur> for the note duration (C<16>
or C<4> or such), the I<note> (C<a>, C<b>, etc), and I<rest> (probably
should be C<r> or C<s>).

=item B<to_midi> I<count> [ I<param> ]

Encodes I<count> measures of the replay log as MIDI and returns a
L<MIDI::Track> object. Parameters include I<chan>, I<dur>, I<note>,
I<tempo>, I<velo> (see L<MIDI::Event>) as well as the I<sustain> and
I<notext> booleans.

I<sustain> holds notes open until the next onset while I<notext> removes
the C<text_event> that document where each new C<pattern,ttl> pair
begins. I<sustain> may result in the final note of the track having a
different duration than in other repeats of the same measure.

Enabling I<notext> will increase the end-of-track raggedness; a MIDI
C<text_event> is used to demark where the track ends that I<notext>
will remove.

=back

=head1 CALLBACK

The B<next> callback is passed the object and a set of parameters; these
are either passed in through B<advance> by the caller or are set by
B<advance>. In particular the I<measure> number (counting from 0, not 1)
and the current I<pattern> are set by B<advance>.

=head1 BUGS

B<to_ly> has no support for changing the meter to match the length of
the I<pattern> should that vary over time. In theory this could be as
simple as prefixing a C<\time 5/4> (or whatever) time change for
patterns that work out to five quarter notes (or whatever) before the
notes for the C<[pattern],ttl> pair in question. In practice there are
probably grue waiting to mug you around various sharp corners in any
such implementation, such as when to use C<3/4> versus C<6/8> given
twelve 16th notes...

<https://github.com/thrig/Music-RhythmSet>

=head1 SEE ALSO

L<MIDI>

=head1 COPYRIGHT AND LICENSE

Copyright 2021 Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<https://opensource.org/licenses/BSD-3-Clause>

=cut
