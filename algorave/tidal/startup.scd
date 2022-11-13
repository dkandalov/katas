// Midi setup on macos:
// - open Audio MIDI Setup app
// - double-click on IAC Driver
// - make sure that IAC Drive
// - add ports "IAC Bus 2" and "IAC Bus 3"

MIDIClient.init;

~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
~midiOut2 = MIDIOut.newByName("IAC Driver", "IAC Bus 2");
~midiOut3 = MIDIOut.newByName("IAC Driver", "IAC Bus 3");

SuperDirt.start

~dirt.soundLibrary.addMIDI(\midi1, ~midiOut);
~dirt.soundLibrary.addMIDI(\midi2, ~midiOut2);
~dirt.soundLibrary.addMIDI(\midi3, ~midiOut3);


(
var on, off, cc;
var osc;

osc = NetAddr.new("127.0.0.1", 6010);

MIDIClient.init;
MIDIIn.connectAll;

on = MIDIFunc.noteOn({ |val, num, chan, src|
    osc.sendMsg("/ctrl", num.asString, val/127);
});

off = MIDIFunc.noteOff({ |val, num, chan, src|
    osc.sendMsg("/ctrl", num.asString, 0);
});

cc = MIDIFunc.cc({ |val, num, chan, src|
    osc.sendMsg("/ctrl", num.asString, val/127);
});

if (~stopMidiToOsc != nil, {
    ~stopMidiToOsc.value;
});

~stopMidiToOsc = {
    on.free;
    off.free;
    cc.free;
};
)