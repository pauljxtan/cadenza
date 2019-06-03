import './main.css'
import { Elm } from './Main.elm'
// import registerServiceWorker from './registerServiceWorker'

var app = Elm.Main.init({
  node: document.getElementById('root')
})

// Elm subscription: fires on each model update
app.ports.toJs.subscribe(function (data) {
  render(JSON.parse(data))
})

// For PWA caching
// registerServiceWorker()

// ---- VexFlow stuff below

const TOTAL_NOTES_WIDTH = 600
const SVG_WIDTH = 650
const SVG_HEIGHT = 100
const OCTAVE_NOTES = ['C', 'D', 'E', 'F', 'G', 'A', 'B']
const CHORD_SYMBOLS = ['o7', 'ø7', 'm7', 'mM7', '7', 'M7', '+7', '+M7']

var VF = Vex.Flow
var div = document.getElementById('staff')

// Render an empty staff
render('')

/** Draws notes and chord symbols on the staff. */
function render (data) {
  // TODO: There may be a better way to clear the staff instead of
  // re-rendering everything from scratch each time
  div.innerHTML = ''

  var [context, stave] = emptyStave()

  // Nothing to draw
  if (data === '' || data['key'] === '') return

  var voice = new VF.Voice({ num_beats: 32, beat_value: 4 })
  voice.addTickables(getChords(data))

  var voice2 = new VF.Voice({ num_beats: 32, beat_value: 4 })
  var text = getSymbols(data['key'], stave)
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
    .setContext(context)
    .draw()

  return [context, stave]
}

/** Returns an array of StaveNote objects corresponding to chords. */
function getChords (data) {
  const chords = data['chords']

  return Object.keys(chords).map(function (chordName) {
    const elems = chords[chordName].split(',')
    const notes = elems.slice(0, 4)
    const accs = elems.slice(4, 8)

    // A bit of logic is needed to make the notes line up properly across octaves
    var octave = 4
    var keys = notes.map(function (note) {
      if (octave === 4 && OCTAVE_NOTES.indexOf(note) < OCTAVE_NOTES.indexOf(notes[0])) { octave += 1 }
      return `${note}/${octave}`
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
  return CHORD_SYMBOLS.map(function (chord) {
    return new VF.TextNote({ text: key + chord, duration: 'w' })
      .setLine(1)
      .setStave(stave)
      .setJustification(VF.TextNote.Justification.LEFT)
  })
}

/** Replaces accidentals with nicer-looking unicode equivalents.
 * Note that this must be done separately on both the Elm and JS sides, since
 * VexFlow expects "b" and "#" for accidentals. */
function unicodeAccidentals (text) {
  return text.replace('bb', '𝄫').replace('b', '♭').replace('##', '𝄪').replace('#', '♯')
}
