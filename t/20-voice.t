#!perl
#
# if AUTHOR_TEST_JMATES_MIDI is set to a program the generated MIDI
# files will be passed to that program as the first argument:
#
#   AUTHOR_TEST_JMATES_MIDI=timidity prove t/20-voice.t
#
# the MIDI files can be inspected with `midiutil dump ...` (from my
# scripts repository) which is a thin wrapper around MIDI::Opus ->dump

use 5.24.0;
use Data::Dumper;
use Test::Most tests => 65;
my $deeply = \&eq_or_diff;

use Music::RhythmSet::Util qw(write_midi);
use Music::RhythmSet::Voice;

my @playback;

my $replay =
  [ [ [qw/1 1 0/], 2 ], [ [qw/1 1 1/], 1 ], [ [qw/0 1/], 3 ] ];

sub domidi {
    my ($file, $fn) = @_;
    unlink $file if -f $file;
    $fn->($file);
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    ok(-f $file);
    push @playback, $file;
}

my $voice = Music::RhythmSet::Voice->new(replay => $replay);

# ->stash attribute (to confirm that it exists; it is not used by
# modules in this distribution)
$voice->stash(42);
is($voice->stash, 42);

# lilypond
is($voice->to_ly(4), <<'EOLY');
  % v xx. 2
  c16 c16 r16
  c16 c16 r16
  % v xxx 1
  c16 c16 c16
  % v .x 1
  r16 c16
EOLY

# ->clone, and optional arguments to to_ly
{
    lives_ok {
        my $x = Music::RhythmSet::Voice->new;
        $x->replay(undef);
        $x->clone(undef);
    };

    my $v2 = $voice->clone(42);
    is($v2->to_ly(1, dur => 8, note => 'bes', rest => 's'), <<'MORLOCK');
  % v42 xx. 1
  bes8 bes8 s8
MORLOCK

    my $ref = $v2->replay;
    $ref->[0][0] = 'was it a deep clone?';
    $deeply->($voice->replay, $replay);

    # never was set in the original object
    my $v3 = $voice->clone;
    is($v3->id, undef);

    # measures with varied beats and thus \time support (such as it is)
    my $v4 = Music::RhythmSet::Voice->new(
        replay => [ [ [ 1, 1, 0 ], 1 ], [ [ 1, 0, 1, 0 ], 1 ] ]);
    is($v4->to_ly(2, dur => 4, time => 4), <<'EOLY');
  % v xx. 1
  \time 3/4
  c4 c4 r4
  % v x.x. 1
  \time 4/4
  c4 r4 c4 r4
EOLY
}

# ->to_string
{
    is($voice->to_string(6), "0\t\txx.\t2\n6\t\txxx\t1\n9\t\t.x\t3\n");

    my $v2 = $voice->clone(7);
    is( $v2->to_string(4, rs => "\r", sep => ' '),
        "0 7 xx. 2\r6 7 xxx 1\r9 7 .x 1\r"
    );
}

# ->advance (and thus the next callback)
{
    my $v2 = Music::RhythmSet::Voice->new(
        next => sub {
            my ($self, %param) = @_;
            if ($param{measure} == 0) {
                return [qw/1 1 0/], 2;
            } elsif ($param{measure} == 2) {
                return [qw/1 1 1/], 1;
            } elsif ($param{measure} == 3) {
                return [qw/0 1/], 3;
            } elsif ($param{measure} == 6) {
                return [qw/0 0 1/], 2;
            } else {
                diag Dumper \%param;
                die "should not be reached";
            }
        }
    );
    is($v2->measure, 0);
    is($v2->ttl,     0);

    lives_ok { $v2->advance(6) };

    is($v2->measure, 6);
    $deeply->($v2->replay, $replay);

    $deeply->($v2->pattern, [qw/0 1/]);
    is($v2->ttl, 1);

    lives_ok { $v2->advance };
    $deeply->($v2->pattern, [qw/0 0 1/]);
    is($v2->measure, 7);
    is($v2->ttl,     2);
}

# to_midi is the most likely to be buggy. and also is the most difficult
# to test for correctness
sub track_stats {
    my ($track) = @_;
    my $duration = 0;
    my $ison;
    my %tmeta;
    for my $e ($track->events_r->@*) {
        $duration += $e->[1] if $ison;
        if ($e->[0] eq 'note_on') {
            $tmeta{dangling_on}++ if $ison;
            $ison = 1;
            $tmeta{ $e->[0] . '_notes' }{ $e->[3] }++;
            $duration = 0;
        } elsif ($e->[0] eq 'note_off') {
            $tmeta{dangling_off}++ unless $ison;
            $tmeta{note_on_dur} += $duration;
            $tmeta{ $e->[0] . '_notes' }{ $e->[3] }++;
            $ison = 0;
        }
        $tmeta{ $e->[0] }++;
        $tmeta{_events}++;
        $tmeta{_dur} += $e->[1];
    }
    $tmeta{dangling_on}++ if $ison;
    return \%tmeta;
}

sub audit_track {
    my ($fn, $stats) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my $track;
    lives_ok { $track = $fn->() };
    $deeply->(track_stats($track), $stats);
    return $track;
}

# one measure with two notes at the default duration (20) -- 1 1 0
# (a trailing text_event pads out the duration of the rest)
audit_track(
    sub { $voice->to_midi(1) },
    {   '_dur'           => 60,
        '_events'        => 8,
        'note_off'       => 2,
        'note_off_notes' => { 60 => 2 },
        'note_on'        => 2,
        'note_on_dur'    => 40,
        'note_on_notes'  => { 60 => 2 },
        'set_tempo'      => 1,
        'text_event'     => 2,
        'track_name'     => 1
    }
);

# fiddle with various defaults (see MIDI::Event). _dur is lower than in
# the previous as the notext flag has removed the text_event that would
# pad the track out to three beats
my $track = audit_track(
    sub {
        $voice->to_midi(
            1,
            chan   => 7,
            dur    => 21,
            note   => 67,
            tempo  => 640_000,
            velo   => 111,
            notext => 1,
        );
    },
    {   '_dur'           => 42,
        '_events'        => 6,
        'note_off'       => 2,
        'note_off_notes' => { 67 => 2 },
        'note_on'        => 2,
        'note_on_dur'    => 42,
        'note_on_notes'  => { 67 => 2 },
        'set_tempo'      => 1,
        'track_name'     => 1
    }
);
for my $e ($track->events_r->@*) {
    if ($e->[0] eq 'set_tempo') {
        is($e->[2], 640_000);
    } elsif ($e->[0] eq 'note_on') {
        is($e->[2], 7);
        is($e->[4], 111);
        last;
    }
}
if (defined $ENV{AUTHOR_TEST_JMATES_MIDI}) {
    my $file = 't/twogs.midi';
    write_midi($file, $track);
    push @playback, $file;
}

# more MIDI - 1 1 0  1 1 0   1 1 1   0 1  0 1  0 1
$track = audit_track(
    sub { $voice->to_midi(6, dur => 96, sustain => 1) },
    {   '_dur'           => 1440,           # (3*2 + 3 + 2*3) * 96
        '_events'        => 26,
        'note_off'       => 10,
        'note_off_notes' => { 60 => 10 },
        'note_on'        => 10,
        'note_on_dur'    => 1440,
        'note_on_notes'  => { 60 => 10 },
        'set_tempo'      => 1,
        'text_event'     => 4,
        'track_name'     => 1
    }
);

write_midi('t/sustains.midi', $track);
write_midi('t/staccato.midi', $voice->to_midi(6, dur => 96));

push @playback, 't/sustains.midi', 't/staccato.midi';

# a whole lot of nothing
$track = audit_track(
    sub {
        Music::RhythmSet::Voice->new(
            id      => 42,
            pattern => [0],
            ttl     => 1
        )->to_midi(1);
    },
    {   '_dur'       => 20,
        '_events'    => 4,
        'set_tempo'  => 1,
        'text_event' => 2,
        'track_name' => 1
    }
);
write_midi('t/silence-pa.midi', $track);

# less nothing
$track = audit_track(
    sub {
        Music::RhythmSet::Voice->new(pattern => [0], ttl => 1)->to_midi(1, notext => 1);
    },
    {   '_dur'       => 0,
        '_events'    => 2,
        'set_tempo'  => 1,
        'track_name' => 1
    }
);
write_midi('t/silence-re.midi', $track);

# more nothing
$track = audit_track(
    sub {
        Music::RhythmSet::Voice->new(next => sub { [qw/0 0 0/], 2 })->advance(4)
          ->to_midi(3);
    },
    {   '_dur'       => 180,
        '_events'    => 5,
        'set_tempo'  => 1,
        'text_event' => 3,
        'track_name' => 1
    }
);
write_midi('t/silence-ci.midi', $track);

# test various conditions to up the coverage; run
#   ./Build testcover
# to see what is being missed
lives_ok { Music::RhythmSet::Voice->new(ttl     => 0) };
lives_ok { Music::RhythmSet::Voice->new(pattern => [1]) };
dies_ok {
    Music::RhythmSet::Voice->new(pattern => undef, ttl => 0)
};
dies_ok {
    Music::RhythmSet::Voice->new(pattern => undef, ttl => 42)
};
dies_ok {
    Music::RhythmSet::Voice->new(pattern => [1], ttl => 0)
};
dies_ok {
    Music::RhythmSet::Voice->new(pattern => {}, ttl => 42)
};

# no or an invalid 'next' callback
$voice = Music::RhythmSet::Voice->new;
dies_ok { $voice->advance };
$voice->next({});
dies_ok { $voice->advance };

# invalid return values from a 'next' callback
$voice->next(sub { [1], -5000 });
dies_ok { $voice->advance };
$voice->next(sub { undef, 42 });
dies_ok { $voice->advance };
$voice->next(sub { {}, 42 });
dies_ok { $voice->advance };
$voice->next(sub { [], 42 });
dies_ok { $voice->advance };

# -> clone does some error checking
$voice = Music::RhythmSet::Voice->new(ttl => 42);
$voice->pattern({});
dies_ok { $voice->clone };
$voice->pattern([1]);
$voice->replay({});
dies_ok { $voice->clone };
$voice->replay([ {}, {} ]);
dies_ok { $voice->clone(99) };

# need a measure count
dies_ok { $voice->to_ly };
dies_ok { $voice->to_ly(-1) };
dies_ok { $voice->to_midi };
dies_ok { $voice->to_midi(-1) };
dies_ok { $voice->to_string };
dies_ok { $voice->to_string(-1) };

# replay log is no bueno, or empty (did you forget to call ->advance to
# populate it? asking for a friend)
$voice = Music::RhythmSet::Voice->new;
$voice->replay(undef);
dies_ok { $voice->to_ly(1) };
dies_ok { $voice->to_midi(1) };
dies_ok { $voice->to_string(1) };
$voice->replay({});
dies_ok { $voice->to_ly(1) };
dies_ok { $voice->to_midi(1) };
dies_ok { $voice->to_string(1) };
$voice->replay([]);
dies_ok { $voice->to_ly(1) };
dies_ok { $voice->to_midi(1) };
dies_ok { $voice->to_string(1) };

if (defined $ENV{AUTHOR_TEST_JMATES_MIDI}) {
    diag "playback ...";
    for my $file (@playback) {
        diag "playing $file ...";
        system $ENV{AUTHOR_TEST_JMATES_MIDI}, $file;
    }
}
