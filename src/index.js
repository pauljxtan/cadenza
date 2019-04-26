import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

var app = Elm.Main.init({
    node: document.getElementById("root")
});

app.ports.toJs.subscribe(function(str) {
    render(JSON.parse(str));
});

registerServiceWorker();

var VF = Vex.Flow;
var div = document.getElementById("staff");
render("");

function render(data) {
    div.innerHTML = "";

    var [context, stave] = emptyStave();

    // Nothing to draw
    if (data == "" || data["Augmented"].startsWith(",,,")) return;

    var notes = getNotes(data);

    var voice = new VF.Voice({ num_beats: 32, beat_value: 4 });
    voice.addTickables(notes);

    // Justify notes
    var formatter = new VF.Formatter().joinVoices([voice]).format([voice], 500);

    voice.draw(context, stave);
}

function emptyStave() {
    var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);

    // Size of SVG
    renderer.resize(575, 150);
    var context = renderer.getContext();

    var stave = new VF.Stave(0, 0, 575)
        .addClef("treble")
        .addTimeSignature("4/4")
        .setContext(context)
        .draw();
    return [context, stave];
}

// TODO: Make this function less hacky
function getNotes(data) {
    return Object.keys(data).map(function(chordName) {
        var notes = data[chordName].split(",").slice(0, 4);
        var accs = data[chordName].split(",").slice(4, 8);

        var keys = [];
        var octave = 4;
        var raised = false;
        var tonic = notes[0];

        for (var i = 0; i < 4; i++) {
            if (!raised && i == 1 && ["B", "A"].includes(tonic)) {
                octave += 1;
                raised = true;
            }
            if (!raised && i == 2 && ["B", "A", "G", "F"].includes(tonic)) {
                octave += 1;
                raised = true;
            }
            if (
                !raised &&
                i == 3 &&
                ["B", "A", "G", "F", "E", "D"].includes(tonic)
            ) {
                octave += 1;
                raised = true;
            }
            keys.push(notes[i] + "/" + octave);
        }

        var note = new VF.StaveNote({
            clef: "treble",
            keys: keys,
            duration: "w"
        });

        for (var i = 0; i < 4; i++) {
            if (accs[i] != "") {
                note = note.addAccidental(i, new VF.Accidental(accs[i]));
            }
        }
        return note;
    });
}
