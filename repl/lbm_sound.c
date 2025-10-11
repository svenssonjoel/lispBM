/*
    Copyright 2025    Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
  Audio generation gotchas to be aware of:
  
  aliasing: a manifestation of a lower frequency that
            that happens when the sampled sound frequency is above half of the
            sampling frequency.
            - Remedied by selecting a sampling frequency such that it is at least 2x the audible spectrum.
            - For more info see: https://en.wikipedia.org/wiki/Aliasing

            Square waves contains all odd harmonics and will lead to
            aliasing in the audible range.
            - Remedied by generating the square wave at a much higher sampling
               frequency then doing a low-pass filter and a downsampling.
            - Remedied by a wave generation method called BLEP:
               https://www.martin-finke.de/articles/audio-plugins-018-polyblep-oscillator/
            - Remedied by approximating a square by adding together
               odd harmonics up to the Nyquist frequency.
               odd harmonics are found at 1x 3x 5x ... the note base frequency.

  beating: Interplay of multiple sine waves close in frequency (half-steps apart)
           where they are periodically amplifying each other causing a
           2-stroke engine (or hellicopter blade) like beating sound.
           The frequency of the emerging beat is |freq1 - freq2|.
           (For example C at 261.63Hz and C# at 277.18 pure sine waves
            result in a beat at 15.55Hz)
           - Remedied by not using entirely pure sine waves, introduce more harmonics.
           - For more details: https://en.wikipedia.org/wiki/Beat_(acoustics)

  phase synchronization:
           Multiple simultaneous voices all starting at phase 0.0
           will constructively interfere (amplify each other) potentially
           leading to clipping when they sum up above the range of the sample
           width (number of bits 16 in our case).
           - Remedied by randomizing phase at note-on.
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <alloca.h>

#include <math.h>

#include <extensions.h>
#include <platform_thread.h>
#include <platform_timestamp.h>
#include "lbm_sound.h"

#include <alsa/asoundlib.h>

static snd_pcm_t *pcm_handle = NULL;

static lbm_thread_t audio_thread;
static bool audio_thread_running = false;

static uint32_t voice_sequence_number = 0;

// Default audio parameters
#define SAMPLE_RATE 44100
#define CHANNELS 2
#define BITS_PER_SAMPLE 16
#define LATENCY_US 30000
#define BUFFER_FRAMES 512

// frames = 128 => ~2.9ms per buffer at 44.1kHz

#define MAX_PATCHES 128
#define MAX_VOICES  16
#define NUM_MODULATORS 4

#define NUM_LFO 2
#define NUM_OSC 2

#define WRAP1(X) if ((X) >= 1.0f) (X) -= 1.0f


#ifndef M_PI
#define M_PI 3.14159265f
#endif

// Oscilator types
typedef enum {
  OSC_NONE = 0,
  OSC_SINE,
  OSC_SAW,
  OSC_TRIANGLE,
  OSC_SQUARE
} oscillator_type_t;

typedef enum {
  MOD_NONE = 0,
  MOD_LFO1,
  MOD_LFO2,
  MOD_ENV,
  MOD_VELOCITY
} modulator_source_t;

typedef struct {
  modulator_source_t source;
  float amount;
} modulator_t;

typedef enum {
  FREQ_NOTE = 0,
  FREQ_FIXED
} freq_source_t;

// In every iteration, an oscillators frequency is computed as: 
//  b + (m0 * a0) + (m1 * a1) .. + (m3 * a3)
// where b is base frequency from note value.
typedef struct {
  oscillator_type_t type;
  freq_source_t freq_source;
  float freq_value; // Repurpose depending on freq source.
  modulator_t modulators[NUM_MODULATORS];
  float phase_offset;
  float vol;
} oscillator_t;
  
typedef enum {
  ENV_OFF = 0,
  ENV_ATTACK,
  ENV_DECAY,
  ENV_SUSTAIN,
  ENV_RELEASE
} env_adsr_state_t;

// Attack, decay, sustan, release
typedef struct {
  float attack_time;
  float decay_time;
  float sustain_level;
  float release_time;
} env_adsr_t;

// A synthesizer patch (Instrument)
typedef struct {
  oscillator_t lfo[NUM_LFO];
  oscillator_t osc[NUM_OSC];
  env_adsr_t   env;
} patch_t; 

typedef struct {
  uint32_t sequence_number; // Steal the oldest
  bool active;
  uint8_t patch;
  uint8_t note; // Midi note id.
  float   freq;
  float   vel;

  //dynamic state during playback;
  float osc_phase[NUM_OSC];
  float lfo_phase[NUM_LFO];
  float env_val;
  env_adsr_state_t env_state;
  float env_time_in_state;
  float release_start_level; // Envelope level when release was triggered 
} voice_t;


static patch_t patches[MAX_PATCHES];
static voice_t voices[MAX_VOICES];

// Update envelope state and return current envelope value
static float update_envelope(voice_t *voice, patch_t *patch) {
  float time_delta = 1.0f / SAMPLE_RATE;  // Time per sample
  voice->env_time_in_state += time_delta;

  switch (voice->env_state) {
  case ENV_OFF:
    voice->env_val = 0.0f;
    break;

  case ENV_ATTACK:
    if (patch->env.attack_time > 0.0f) {
      voice->env_val = voice->env_time_in_state / patch->env.attack_time;
      if (voice->env_val >= 1.0f) {
        voice->env_val = 1.0f;
        voice->env_state = ENV_DECAY;
        voice->env_time_in_state = 0.0f;
      }
    } else {
      // Instant attack
      voice->env_val = 1.0f;
      voice->env_state = ENV_DECAY;
      voice->env_time_in_state = 0.0f;
    }
    break;

  case ENV_DECAY:
    if (patch->env.decay_time > 0.0f) {
      float progress = voice->env_time_in_state / patch->env.decay_time;
      voice->env_val = 1.0f - (1.0f - patch->env.sustain_level) * progress;
      if (progress >= 1.0f) {
        voice->env_val = patch->env.sustain_level;
        voice->env_state = ENV_SUSTAIN;
        voice->env_time_in_state = 0.0f;
      }
    } else {
      // Instant decay
      voice->env_val = patch->env.sustain_level;
      voice->env_state = ENV_SUSTAIN;
      voice->env_time_in_state = 0.0f;
    }
    break;

  case ENV_SUSTAIN:
    voice->env_val = patch->env.sustain_level;
    break;

  case ENV_RELEASE:
    if (patch->env.release_time > 0.0f) {
      float progress = voice->env_time_in_state / patch->env.release_time;
      // Release from the level stored when release was triggered
      voice->env_val = voice->release_start_level * (1.0f - progress);

      if (progress >= 1.0f) {
        voice->env_val = 0.0f;
        voice->env_state = ENV_OFF;
        voice->active = false;
      }
    } else {
      // Instant release
      voice->env_val = 0.0f;
      voice->env_state = ENV_OFF;
      voice->active = false;
    }
    break;
  }

  return voice->env_val;
}

static void audio_generation_thread(void *arg) {
  (void)arg;

  int16_t *buffer = (int16_t *)malloc(BUFFER_FRAMES * CHANNELS * sizeof(int16_t));
  if (!buffer) {
    fprintf(stderr, "Failed to allocate audio buffer\n");
    return;
  }

  while (audio_thread_running) {

    memset(buffer, 0, BUFFER_FRAMES * CHANNELS * sizeof(int16_t));
    
    for (int i = 0; i < BUFFER_FRAMES; i ++) {
      float s_left = 0.0;
      float s_right = 0.0;

      for (int v = 0; v < MAX_VOICES; v ++) {
        if (voices[v].active) {
          
          float base_freq = voices[v].freq;
          float vel = voices[v].vel * 24000.0f;

          float env_val = update_envelope(&voices[v], &patches[voices[v].patch]);
          for (int o = 0; o < NUM_OSC; o ++) {

            oscillator_t *w = &patches[voices[v].patch].osc[o];
            float phase = voices[v].osc_phase[o] + w->phase_offset;
            WRAP1(phase);

            float s = 0.0f;
            
            switch (w->type) {
            case OSC_SAW: {
              // The saw wave jumps from 1.0 to -1.0
              // instantaneoulsy => lots of harmonics => aliasing
              float osc = 2.0f * phase - 1.0f; 
              s = osc * env_val * vel * w->vol;

              float phase_increment = base_freq / 44100.0f;
              voices[v].osc_phase[o] += phase_increment;
              WRAP1(voices[v].osc_phase[o]);
              s_left  += s; 
              s_right += s;
            } break;
            case OSC_SINE: {              
              // TODO: check if modulator and modulate phase_increment (I think).
              float osc = sinf(2.0f * M_PI * phase);
              float s = osc * env_val * vel *w->vol;

              // In the future use the modulated frequency here
              float phase_increment = base_freq / 44100.0f;
              voices[v].osc_phase[o] += phase_increment;
              WRAP1(voices[v].osc_phase[o]);

              s_left  += s; // for now the same.
              s_right += s; // may change in future
            } break;
            default:
              break;
            }
          }
          
        }    
      }

      // tanh based "soft clipping"
      // tanh(x) approaches 1.0 as x grows towards infinity
      float mixed_l = tanhf(s_left / 30000.0f);
      float mixed_r = tanhf(s_right / 30000.0f);
      
      buffer[i*2]   = (int16_t)(mixed_l * 32767.0);
      buffer[i*2+1] = (int16_t)(mixed_r * 32767.0);
    }
 
    // snd_pcm_writei blocks when the internal ALSA buffer is full
    // This is the only synchronization we will use in this thread.
    // Important to remember this when eventually writing and embedded
    // variant of this code!
    snd_pcm_sframes_t frames = snd_pcm_writei(pcm_handle, buffer, BUFFER_FRAMES);

    // Normally frames will be equal to BUFFER_FRAMES (or negative).
    // A partial write will not happen using the blocking snd_pcm_writei (normally).
    
    if (frames < 0) {
      frames = snd_pcm_recover(pcm_handle, frames, 0);
      if (frames < 0) {
        printf("snd_pcm_writei failed: %s\n", snd_strerror(frames));
        break;
      }
    }
  }

  free(buffer);
  printf("Audio generation thread stopped\n");
}

// ////////////////////////////////////////////////////////////
// LBM symbols

static lbm_uint sym_attack  = 0;
static lbm_uint sym_decay   = 0;
static lbm_uint sym_sustain = 0;
static lbm_uint sym_release = 0;

static lbm_uint sym_envelope = 0;

static lbm_uint sym_freq_src_note = 0;
static lbm_uint sym_freq_src_fixed = 0;

static lbm_uint sym_osc_none     = 0;
static lbm_uint sym_osc_sine     = 0;
static lbm_uint sym_osc_saw      = 0;
static lbm_uint sym_osc_triangle = 0;
static lbm_uint sym_osc_square   = 0;

static lbm_uint sym_osc1 = 0;
static lbm_uint sym_osc2 = 0;
static lbm_uint sym_lfo1 = 0;
static lbm_uint sym_lfo2 = 0;

static lbm_uint sym_mod_none = 0;
static lbm_uint sym_mod_lfo1 = 0;
static lbm_uint sym_mod_lfo2 = 0;
static lbm_uint sym_mod_env  = 0;
static lbm_uint sym_mod_vel  = 0;

static lbm_uint sym_mod1 = 0;
static lbm_uint sym_mod2 = 0;
static lbm_uint sym_mod3 = 0;
static lbm_uint sym_mod4 = 0;

static void register_symbols(void) {
  lbm_add_symbol("attack", &sym_attack);
  lbm_add_symbol("decay" , &sym_decay);
  lbm_add_symbol("sustain", &sym_sustain);
  lbm_add_symbol("release", &sym_release);

  lbm_add_symbol("envelope", &sym_envelope);

  lbm_add_symbol("freq-src-note", &sym_freq_src_note);
  lbm_add_symbol("freq-src-fixed", &sym_freq_src_fixed);
  
  lbm_add_symbol("osc-none", &sym_osc_none);
  lbm_add_symbol("osc-sine", &sym_osc_sine);
  lbm_add_symbol("osc-saw", &sym_osc_saw);
  lbm_add_symbol("osc-triangle", &sym_osc_triangle);
  lbm_add_symbol("osc-square", &sym_osc_square);

  lbm_add_symbol("osc1", &sym_osc1);
  lbm_add_symbol("osc2", &sym_osc2);
  lbm_add_symbol("lfo1", &sym_lfo1);
  lbm_add_symbol("lfo2", &sym_lfo2);

  lbm_add_symbol("mod-none", &sym_mod_none);
  lbm_add_symbol("mod-lfo1", &sym_mod_lfo1);
  lbm_add_symbol("mod-lfo2", &sym_mod_lfo2);
  lbm_add_symbol("mod-env", &sym_mod_env);
  lbm_add_symbol("mod-vel", &sym_mod_vel);

  lbm_add_symbol("mod1", &sym_mod1);
  lbm_add_symbol("mod2", &sym_mod2);
  lbm_add_symbol("mod3", &sym_mod3);
  lbm_add_symbol("mod4", &sym_mod4);
}



// ////////////////////////////////////////////////////////////
// LBM extensions
// (patch-osc-set-tvp p-num o-num o-type o-vol o-phase-offset)
lbm_value ext_patch_osc_set_tvp(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;

  if (argn == 5 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_symbol(args[2]) &&
      lbm_is_number(args[3]) &&
      lbm_is_number(args[4])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    float vol = lbm_dec_as_float(args[3]);
    float phase_offset = lbm_dec_as_float(args[4]);
    lbm_uint osc_type = lbm_dec_sym(args[2]);
    uint32_t osc = lbm_dec_as_u32(args[1]);

    if (osc < NUM_OSC && patch < MAX_PATCHES) {

      oscillator_type_t o;
      if (osc_type == sym_osc_none) {
        o = OSC_NONE;     
      } else if (osc_type == sym_osc_sine) {
        o = OSC_SINE;
      } else if (osc_type == sym_osc_saw) {
        o = OSC_SAW;
      } else if (osc_type == sym_osc_triangle) {
        o = OSC_TRIANGLE;
      } else if (osc_type == sym_osc_square) {
        o = OSC_SQUARE;
      }
      patches[patch].osc[osc].type = o;
      patches[patch].osc[osc].freq_source = FREQ_NOTE; // by default
      patches[patch].osc[osc].vol = vol;
      patches[patch].osc[osc].phase_offset = phase_offset;
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}


lbm_value ext_patch_adsr_get(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0])) {
    
    uint8_t patch = lbm_dec_as_char(args[0]);
    lbm_value a = lbm_enc_float(patches[patch].env.attack_time);
    lbm_value d = lbm_enc_float(patches[patch].env.decay_time);
    lbm_value s = lbm_enc_float(patches[patch].env.sustain_level);
    lbm_value rel = lbm_enc_float(patches[patch].env.release_time);
    if (a == ENC_SYM_MERROR ||
        d == ENC_SYM_MERROR ||
        s == ENC_SYM_MERROR ||
        rel == ENC_SYM_MERROR) {
      r = ENC_SYM_MERROR;
      } else {
      r = lbm_heap_allocate_list_init(4, a, d, s ,rel);
    }
  }
  return r;
}

lbm_value ext_patch_clear(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_number(args[0])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    if (patch < MAX_PATCHES) {
      memset(&patches[patch], 0, sizeof(patch_t));
      r = ENC_SYM_TRUE;
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

// (set-adsr patch-no attack decay sustain-level release) 
lbm_value ext_patch_adsr_set(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 5 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_number(args[3]) &&
      lbm_is_number(args[4])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    float   attack = lbm_dec_as_float(args[1]);
    float   decay  = lbm_dec_as_float(args[2]);
    float   sustain = lbm_dec_as_float(args[3]);
    float   release = lbm_dec_as_float(args[4]); 

    patches[patch].env.attack_time = attack;
    patches[patch].env.decay_time = decay;
    patches[patch].env.sustain_level = sustain;
    patches[patch].env.release_time = release;
    r = ENC_SYM_TRUE;
  }
  return r;
}

static void start_voice(voice_t *v, uint8_t patch, uint8_t note, float freq, float vel) {
  v->sequence_number = voice_sequence_number ++;
  v->patch = patch;
  v->note = note;
  v->freq = freq;
  v->vel = vel;

  float phase = (float)rand() / RAND_MAX;
  v->osc_phase[0] = phase;
  v->osc_phase[1] = phase;
  v->lfo_phase[0] = 0.0f;
  v->lfo_phase[1] = 0.0f;
  v->env_val = 0.0f;
  v->env_state = ENV_ATTACK;
  v->env_time_in_state = 0.0f;
  v->release_start_level = 0.0f;
  v->active = true;
}

// (note-on patch-no note-id vel)
lbm_value ext_note_on(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 3 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_number(args[2])) {

    uint8_t patch = lbm_dec_as_char(args[0]);
    uint8_t note = lbm_dec_as_char(args[1]);
    uint8_t vel = lbm_dec_as_char(args[2]);
    float vel_f = (float)vel / 127.0; 
    float freq = 440.0f * powf(2.0f, (note - 69) / 12.0f);

    // replacement strategy needed.
    // But need to keep track of which one to replace then..
    uint32_t min_seq = UINT32_MAX;
    int      min_seq_ix = -1;
    int  slot_ix = -1;
    for (int i = 0; i < MAX_VOICES; i ++) {
      if (min_seq > voices[i].sequence_number) {
        min_seq = voices[i].sequence_number;
        min_seq_ix = i;
      }
      if (voices[i].active &&
          voices[i].patch == patch &&
          voices[i].note  == note) {
        slot_ix = i;
        break;
      }
      if (!voices[i].active) slot_ix = i;
    }
    if (slot_ix < 0) slot_ix = min_seq_ix;
    start_voice(&voices[slot_ix], patch, note, freq, vel_f);
    r = ENC_SYM_TRUE;
  }
  return r;
}

// (note-off patch-no node-id)
lbm_value ext_note_off(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_number(args[1])) {
    uint8_t patch = lbm_dec_as_char(args[0]);
    uint8_t note = lbm_dec_as_char(args[1]);

    for (int i = 0; i < MAX_VOICES; i ++) {
      if (voices[i].active &&
          voices[i].patch == patch &&
          voices[i].note  == note) {
        voices[i].env_state = ENV_RELEASE;
        voices[i].env_time_in_state = 0.0f;
        voices[i].release_start_level = voices[i].env_val;
        break;
      }    
    }
    r = ENC_SYM_TRUE;
  }
  return r;
}

// ////////////////////////////////////////////////////////////
// Init

bool lbm_sound_init(void) {
  int err;

  // plughw:0,0 requests direct to hardware control
  // This will fail if some other application is playing sound.
  //err = snd_pcm_open(&pcm_handle, "plughw:0,0", SND_PCM_STREAM_PLAYBACK, 0);
  err = snd_pcm_open(&pcm_handle, "default", SND_PCM_STREAM_PLAYBACK, 0);
  if (err < 0) {
    fprintf(stderr, "Cannot open audio device: %s\n", snd_strerror(err));
    return false;
  }

  err = snd_pcm_set_params(pcm_handle,
                           SND_PCM_FORMAT_S16_LE,
                           SND_PCM_ACCESS_RW_INTERLEAVED,
                           CHANNELS,
                           SAMPLE_RATE,
                           1,
                           LATENCY_US);
  if (err < 0) {
    fprintf(stderr, "Cannot set audio parameters: %s\n", snd_strerror(err));
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    return false;
  }

  printf("ALSA sound system initialized: %d Hz, %d channels, %d-bit, %dms latency\n",
         SAMPLE_RATE, CHANNELS, BITS_PER_SAMPLE, LATENCY_US / 1000);

  audio_thread_running = true;
  if (!lbm_thread_create(&audio_thread, "audio_gen", audio_generation_thread,
                         NULL, LBM_THREAD_PRIO_HIGH, 32768)) {
    fprintf(stderr, "Failed to create audio generation thread\n");
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    return false;
  }

  for (int i = 0; i < MAX_VOICES; i ++) {
    voices[i].active = false;
  }

  register_symbols();
  
  lbm_add_extension("note-on", ext_note_on);
  lbm_add_extension("note-off", ext_note_off);
  lbm_add_extension("patch-clear", ext_patch_clear);
  lbm_add_extension("patch-osc-set-tvp", ext_patch_osc_set_tvp);
  lbm_add_extension("patch-adsr-set", ext_patch_adsr_set);
  lbm_add_extension("patch-adsr-get", ext_patch_adsr_get);
  

  return true;
}

void lbm_sound_cleanup(void) {
  if (audio_thread_running) {
    audio_thread_running = false;
    lbm_thread_destroy(&audio_thread);
  }

  if (pcm_handle) {
    snd_pcm_drain(pcm_handle);
    snd_pcm_close(pcm_handle);
    pcm_handle = NULL;
    printf("ALSA sound system cleanup complete\n");
  }
}


