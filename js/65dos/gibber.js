// from http://gibber.mat.ucsb.edu/?path=65daysofstatic%2Fpublications%2FyrCodeIsBadAndYouShouldFeelBad
/*
 65                65
 \                /
 \              /
 \            /
 \          /
 \        /
 \      /
 \    /
 \  /
 \/
 ||
 ---------------------------------------------------------------------------------
 |                                                                                 |
 |           *******************                                                   |
 |          *                 *   >>> 65daysofstatic                               |
 |         *  ////// //////  *  >> yrCodeIsBadAndYouShouldFeelBad                  |
 |        *  /      /       *   > noRightsReserved2014                             |
 |       *  ////// //////  *                                                       |
 |      *  /    /      /  *                                                        |
 |     *  ////// //////  *                                                         |
 |    *                 *                                                          |
 |   *******************                                                           |
 |                                                                                 |
 |                                                                                 |
 |    >>> instructions for getting started:                                        |
 |                                                                                 |
 |     1. select all the code in this window (control-a/cmd-a).                    |
 |     2. plug your computer into a massive p.a.                                   |
 |     3. press ctrl-shift-enter to play.                                          |
 |     4. listen.                                                                  |
 |     5. hack it to pieces.                                                       |
 |     6. tweet us @65dos if you rmx it or anything.                               |
 |                                                                                 |
 |     more info on how to use gibber here: http://gibber.mat.ucsb.edu/            |
 |     more info on us here: http://www.65daysofstatic.com * twitter: @65dos       |
 |     h/t @yaxu for pointing us toward gibber in the first place.                 |
 |                                                                                 |
 ---------------------------------------------------------------------------------
 */

Gibber.Clock.maxMeasures = 65;

//***** createInstrumentsFxDoubtAndUncertainty *****

// whiteNoiseAnaesthetic
s = Synth({ maxVoices: 2, waveform: 'Noise', attack: ms(1), decay: ms(1) });

// tightKit
z = XOX('.');
z.snare.snappy = 4;
z.snare.amp = 0.1;
z.hat.amp = 0.3;
z.hat.decay = .3;
z.kick.amp = 1;
z.kick.decay = .2;
z.snare.decay = .1;

// sawtoothBass
// a = Synth({ maxVoices:4, waveform:'Saw', attack:ms(1), decay:ms(500) })
a = Mono({ waveform: 'Saw', cutoff: 0.7, attack: ms(1), filterMult: 0, resonance: 1 });

// pluck
p = Pluck();

// coldSynthAnUndefinableSadnessCheerUpThoughEh?
g = Synth2({ maxVoices: 18, waveform: 'Triangle', attack: ms(600), decay: ms(10000), amp: .8, resonance: 0, cutoff: 0.5, filterMult: 1 });

// chords
one = ["a2", "e2", "a3", "c3", "e3", "a4", "b4", "c4", "e4" ];
two = ["a2", "e2", "a3", "c3", "e3", "c5", "b5", "f4", "e4"];
three = ["c2", "g2", "b3", "c3", "e3", "g3", "c5", "e4", "d4"];
four = ["f2", "c3", "a3", "c3", "e3", "f3", "c5", "f4", "e4"];

// fx (do_fx_dream_of_electronic_affect?)
e = Delay(400, .5);
r = Reverb(.5);
crush = Crush(4, 1);
dist = Distortion(5000);

// putOnYrSmilingMask
p.fx.add(e);
z.fx.add(crush);

// ***** a_sketchy_framework *****

// start_as_you_mean_to_g0_0n

future( function() {
    t = Seq({
        note:[440, 600, 440, 100, 800, 600, 600, 600],
        durations:[1/16],
        amp:[0.05],
        cutoff:[.02, .04, .02, .1, .06, .08, .04, .03],
        pan:[-1, -0.6, -0.4, -0.2, 0, 0.3, 0.6, 1 ],
        target:s
    })

}, 1);

// crushdKiksLikeThe90sUsedToMake

future( function() {

    k = Seq({ note:"x..x.x..x..x.x..x..x.x..x..x.x.x".split(""), durations:1/16, target:z })

}, 2);


// sawBassWhateverYeah

future( function() {

    b = Seq({note: ["a1", "a1", "a1", "a1"],
        amp:  [ .2 ,   .4,   .6,  .5 ],
        cutoff: [ .1, .15, .2, .18 ],
        target: a,
        durations: 1/16
    });

}, 6);

// pluckArp (summon_bored_robot_guitarist)

future( function() {

    d = Seq({
        note: ["A2", "c2", "a3", "a4", "c5", "a3", "a3", "a3", "c5", "a3", "a3", "c5", "a3", "a3", "a1", "a2"],
        amp:  [ .05 ,   .06,   .08,  .07 ],
        target: p,
        damping: [0, 0, 0, 0.4, 1, 0, 0, 0],
        durations: 1/16
    });

}, 8);


// moar_beats && spellProperly (false)

future( function() {

    l = Seq({ note: "...o.....o....................o.".split(""), durations: 1 / 16, target: z });
    m = Seq({ note:"****************".split(""), durations:1/16, target:z })

}, 10);


// coldSynthChords aka redWine

future( function() {

    f = g.chord.seq([one, two, one, three, one, four], 2);
    m = Seq({ note: "-*-**-************-**-********-*".split(""), durations: 1 / 16, target: z });

    b = Seq({note: ["a1", "a1", "a1", "a1"],
        amp:  [ .2 ,   .4,   .6,  .5 ],
        cutoff: [ .1, .2, .4, .3 ],
        target: a,
        durations: 1/16
    });

}, 18);


// switch_to_clean_beats_kill_the_cold_synth_bit_glitchy_but_then_isn't_everything_these_days?_pretend_it's_a_metaphor_for_the_future_ok_ok

future( function() {

    f.seq.stop();
    d.stop();
    z.fx.remove();
    g = Synth2({ maxVoices: 9, waveform: 'Triangle', attack: ms(1000), decay: ms(4000), amp: .8, resonance: 0, cutoff: 0.2, filterMult: 1 });

    b = Seq({note: ["A1", "A1", "a1", "a1", "A1", "A1", "a1", "a1"],
        amp:  [ .3 ,   .0,   0,  .3, 0, 0, 0 ,0 ],
        cutoff: [ .7, .1],
        target: a,
        durations: 1/16
    });

    clave = Clave({amp: .8, pan: [-1, 1]}).play(Rndf(1200, 1600), 1 / 12);

    k = Seq({ note: "x.xx.xxxx..x.x..x.xx..x..x.x.x.x".split(""), durations: 1 / 16, target: z });
    l = Seq({ note: "ooo......o....................o.".split(""), durations: 1 / 16, target: z });
    m = Seq({ note: "***-**********-*".split(""), durations: 1 / 16, target: z });


    t = Seq({
        note:[6900, 700, 440, 11100, 800, 600, 600, 4600],
        durations:[1/16],
        amp:[0.003],
        cutoff:[.09],
        pan:[-1, -0.6, -0.4, -0.2, 0, 0.3, 0.6, 1 ],
        target:s
    })

}, 34);

// more_chords_beats_stop_white_noise_noise_noise

future( function() {

    f = g.chord.seq([one, two, one, three, one, four], 2);
    l.stop();
    m.stop();
    clave.seq.stop();
    b.stop();

    z.kick.fx.add(r);
    z.kick.fx.add(dist);


    k = Seq({ note: "x.............................x.".split(""), durations: 1 / 16, target: z });

    t = Seq({
        note:[440, 600, 440, 100, 800, 600, 600, 600],
        durations:[1/16],
        amp:[0.05],
        cutoff:[.02, .04, .02, .1, .06, .08, .04, .03],
        pan:[-1, -0.6, -0.4, -0.2, 0, 0.3, 0.6, 1 ],
        target:s
    });


}, 42);

// end drunk tempo infinity beats, leaving each other forever.

future( function() {

    Clock.rate = Add(1, Sine(.1, .5)._);

    g.seq.stop();

    b = Seq({note: ["A1", "A1", "a1", "a1", "A1", "A1", "a1", "a1"],
        amp:  [ .3 ,   .0,   0,  .3, 0, 0, 0 ,0 ],
        cutoff: [ .7, .1],
        target: a,
        durations: 1/16
    });

    k = Seq({ note: "x.x.xx..x.xx.x..".split(""), durations: 1 / 16, target: z });
    l = Seq({ note: "o..o..o.o..o..o.o..o..o.o..o..oo".split(""), durations: 1 / 16, target: z });
    m = Seq({ note: "-**-**-*".split(""), durations: 1 / 16, target: z });

    t = Seq({
        note:[2400, 600, 440, 1100, 800, 600, 600, 600],
        durations:[1/32, 1/64, 1/128, 1/128],
        amp:[0.08],
        cutoff:[.2, .04, .02, .1, .06, .5, .04, .03],
        pan:[-1, -0.6, -0.4, -0.2, 0, 0.3, 0.6, 1 , 1, 1, 1, 1],
        target:s
    });

}, 58);