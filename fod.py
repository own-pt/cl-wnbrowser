#!/usr/bin/python3
# -*- encoding: utf-8 -*-

import sys
import json
sys.path.append('/home/chalub/freeling/myfreeling/APIs/python')
sys.path.append('/home/chalub/freeling-3.1/APIs/python')
sys.path.append('/home/fcbr/repos/freeling-3.1/APIs/python')

import freeling
import codecs
import re
import time
from datetime import date

FREELINGDIR = "/usr/local"
DATA = FREELINGDIR+"/share/freeling/"
LANG="en"

freeling.util_init_locale("default") 

op = freeling.maco_options(LANG)
op.set_active_modules(0,1,1,1,1,1,1,1,1,1,0)
op.set_data_files(DATA+LANG+"/usermap.dat",
                  DATA+LANG+"/locucions.dat", 
                  DATA+LANG+"/quantities.dat", 
                  DATA+LANG+"/afixos.dat",
                  DATA+LANG+"/probabilitats.dat", 
                  DATA+LANG+"/dicc.src", 
                  DATA+LANG+"/np.dat",  
                  DATA+"common/punct.dat", 
                  "")
op.set_retok_contractions(True)

la  = freeling.lang_ident(DATA+"common/lang_ident/ident.dat")
mf  = freeling.maco(op)
tk  = freeling.tokenizer(DATA+LANG+"/tokenizer.dat")
sp  = freeling.splitter(DATA+LANG+"/splitter.dat")
tg  = freeling.hmm_tagger(DATA+LANG+"/tagger.dat",1,2)
ner = freeling.ner(DATA+LANG+"/nerc/ner/ner-ab-rich.dat")
nec = freeling.nec(DATA+LANG+"/nerc/nec/nec-ab-rich.dat")
sen = freeling.senses(DATA+LANG+"/senses.dat");
ukb = freeling.ukb(DATA+LANG+"/ukb.dat")
num = freeling.numbers("en", ",", ".")
qt = freeling.quantities("en",DATA+LANG+"/quantities.dat")


def tag (obj):
    out = obj
    l = tk.tokenize(obj["text"])

    ls = sp.split(l,1)
    
    ls = mf.analyze(ls)
    ls = tg.analyze(ls)
    ls = sen.analyze(ls)
    ls = ukb.analyze(ls)
    ls = ner.analyze(ls)
    ls = nec.analyze(ls)
    ls = num.analyze(ls)
    ls = qt.analyze(ls)

    ss = []
    all_tags = []
    all_lemmas = []
    all_senses = []
    for s in ls:
        wss = []
        ws = s.get_words()
        for w in ws:
            an = w.get_analysis()

            a = an[0]

            senses = a.get_senses_string().split("/")
            chosen_sense = ""
            if (len(senses) > 0):
                chosen_sense = senses[0].split(':')[0]
                all_senses.append(chosen_sense)
            all_tags.append(a.get_tag())
            all_lemmas.append(a.get_lemma())
            wse = dict(wordform = w.get_form(),
                       strstart = w.get_span_start(),
                       strfinish = w.get_span_finish(),
                       lemma = a.get_lemma(),
                       tag = a.get_tag(),
                       sense = chosen_sense,
                       senses = senses)
            wss.append(wse)
        ss.append(wss)
    out['analysis'] = ss
    out['lang'] = la.identify_language(obj["text"],["es","pt","en","it","fr","de"])
    return out

f = codecs.open(sys.argv[1], "r", "utf-8" )
s = { 'text': f.read() }


with open(sys.argv[2], 'w') as outfile:
    json.dump(tag(s), outfile, indent=2)
    
