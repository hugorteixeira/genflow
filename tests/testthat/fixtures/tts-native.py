#!/usr/bin/env python3
import json,os,sys,time,wave,subprocess
args=sys.argv[1:]
mode=os.environ.get('GENFLOW_TEST_TTS_MODE','ok')
def value(flag): return args[args.index(flag)+1]
if '--list-backends-json' in args:
    if mode=='malformed':print('{"backends":["voxcpm2-tts"]}');sys.exit(0)
    print(json.dumps({'backends':[] if mode=='unsupported' else [{'name':'voxcpm2-tts','caps':['tts','voice-cloning']}]}));sys.exit(0)
if '--version' in args: print('version : fixture\ngit sha : fixture');sys.exit(0)
record=os.environ.get('GENFLOW_TEST_TTS_RECORD')
if record:
    with open(record,'w') as f: json.dump({'args':args,'cfg':os.environ.get('CRISPASR_VOXCPM2_CFG_VALUE'),'steps':os.environ.get('CRISPASR_VOXCPM2_INFERENCE_STEPS'),'max_len':os.environ.get('CRISPASR_VOXCPM2_MAX_LEN')},f)
if mode=='fail': sys.exit(7)
if mode=='timeout':
    child=subprocess.Popen(['sleep','30'])
    if record:
        with open(record+'.pid','w') as f:f.write(str(child.pid))
    time.sleep(30)
if mode=='missing':sys.exit(0)
print("voxcpm2: tokenized '"+value('--tts')+"'",file=sys.stderr)
path=value('--tts-output')
if mode=='invalid':
    with open(path,'wb') as f: f.write(b'not audio')
    sys.exit(0)
if '--voice' in args:
    if mode=='fallback':print('crispasr[voxcpm2-tts]: failed to load reference audio — falling back to zero-shot',file=sys.stderr)
    elif mode!='unconfirmed':print("crispasr[voxcpm2-tts]: loaded reference audio '"+value('--voice')+"' (1600 samples @ 16 kHz)",file=sys.stderr)
with wave.open(path,'wb') as f:
    f.setnchannels(1);f.setsampwidth(2);f.setframerate(48000);f.writeframes(b'\0\1'*4800)

ext=os.path.splitext(path)[1]
if ext in ['.mp3','.opus','.aac']:
    destination=path+'.wav'
    os.rename(path,destination)
    output=os.path.splitext(path)[0]+'.mp4' if ext in ['.opus','.aac'] else path
    codec={'.mp3':'libmp3lame','.opus':'libopus','.aac':'aac'}[ext]
    subprocess.run(['ffmpeg','-v','error','-i',destination,'-c:a',codec,output],check=True)
    os.unlink(destination)
