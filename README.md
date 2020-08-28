# Haskell-PCMSynth
Generates single channel melodies in Wav using sin function

This project reads from a file, the definition for some music, in terms of the notes, that it plays 
as well as the durations of those notes. (Chords not supported. Only simple melodies.)

As per these parsed notes, the corresponding sound is generated using a periodic wave function, and then 
they are encoded into the lossless .wav format (this was the simplest wrapper for sound data I found)

The format for sampleTex.mujik is 

```
[a1a2..] implies on of (a1,a2,a3.. and so on)
a+  implies one or more of a
a* implies none or more of a 
a? implies one or none of a 

note  := [ABCDEFG](#?)[Integer][WHQEST]
mujik := note*

Here, [ABCDEFG] represents the Root Notes. 
# is obvious, (Flats not supported)
[WHQEST] represents the length of the note
W -> Whole (4 Beats)
H -> Half (2 Beats)
Q -> Quarter (1 Beat)
E -> Eighth (0.5 Beat)
S -> Sixteenth (0.25 Beat)
T -> ThirtySecond (0.125 Beat)

These mappings are conventional.

any valid music reccognized by this program is nothing but a space separated list of 
notes.
```
