import './main.css'
import { Elm } from './Main.elm'
// import registerServiceWorker from './registerServiceWorker'

var app = Elm.Main.init({
  node: document.getElementById('root')
})

app.ports.toJs.subscribe(function (str) {
  render(JSON.parse(str))
})

// For PWA caching
// registerServiceWorker()

// ---- VexFlow stuff below

const TOTAL_NOTES_WIDTH = 600
const SVG_WIDTH = 650
const SVG_HEIGHT = 150
const OCTAVE_NOTES = ['C', 'D', 'E', 'F', 'G', 'A', 'B']

var VF = Vex.Flow
var div = document.getElementById('staff')
render('')

/** Draws notes on the staff */
function render (data) {
  div.innerHTML = ''

  var [context, stave] = emptyStave()

  // Nothing to draw
  if (data === '' || data['Augmented'].startsWith(',,,')) return
  console.log(data['Augmented'])
  var elems = data['Augmented'].split(',')
  var key = unicodeAccidentals(elems[0] + elems[5])

  var voice = new VF.Voice({ num_beats: 32, beat_value: 4 })
  voice.addTickables(getChords(data))

  var voice2 = new VF.Voice({ num_beats: 32, beat_value: 4 })
  var text = getSymbols(key, stave)
  voice2.addTickables(text)

  var voices = [voice, voice2]

  // Justify notes
  var formatter = new VF.Formatter()
  formatter.joinVoices(voices).format(voices, TOTAL_NOTES_WIDTH)

  voice.draw(context, stave)
  text.map(function (t) { t.setContext(context).draw() })
}

/** Returns an empty staff. */
function emptyStave () {
  var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG)

  // Size of SVG
  renderer.resize(SVG_WIDTH, SVG_HEIGHT)
  var context = renderer.getContext()

  var stave = new VF.Stave(0, 0, SVG_WIDTH)
    .addClef('treble')
    // .addTimeSignature('4/4')
    .setContext(context)
    .draw()

  return [context, stave]
}

/** Returns an array of StaveNote objects corresponding to chords. */
function getChords (data) {
  return Object.keys(data).map(function (chordName) {
    const elems = data[chordName].split(',')
    const notes = elems.slice(0, 4)
    const accs = elems.slice(4, 8)

    const tonic = notes[0]

    var keys = []
    var octave = 4

    notes.forEach(function (note) {
      if (octave === 4 && OCTAVE_NOTES.indexOf(note) < OCTAVE_NOTES.indexOf(tonic)) { octave += 1 }
      keys.push(`${note}/${octave}`)
    })

    var chord = new VF.StaveNote({
      clef: 'treble',
      keys: keys,
      duration: 'w'
    })

    // Add accidental if needed to each note
    for (var [i, acc] of accs.entries()) {
      if (acc !== '') { chord = chord.addAccidental(i, new VF.Accidental(acc)) }
    }

    return chord
  })
}

/** Returns an array of TextNote objects corresponding to chord symbols. */
function getSymbols (key, stave) {
  return ['o7', 'ø7', 'm7', 'mM7', '7', 'M7', '+7', '+M7'].map(function (chord) {
    return new VF.TextNote({ text: key + chord, duration: 'w' })
      .setLine(1)
      .setStave(stave)
      .setJustification(VF.TextNote.Justification.LEFT)
  })
}

function unicodeAccidentals(text) {
  return text.replace("bb", "𝄫").replace("b","♭").replace("##","𝄪").replace("#","♯")
}
