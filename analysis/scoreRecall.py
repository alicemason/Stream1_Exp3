#!/usr/bin/env python
# encoding: utf-8
"""
scoreRecall.py

Convert firebase json output into free recall scored output

"""

import json
import csv
import sys
from pprint import pprint

def main(argv=None):

    if (len(sys.argv) != 3):
        print "Please specify input and output file names"
        return 0

    inputFileName = sys.argv[1]
    outputFileName = sys.argv[2]

    with open(inputFileName) as json_data:
        data = json.load(json_data)

    # get output file ready
    test_file = open(outputFileName,'wb')
    csvwriter = csv.writer(test_file)

    csvwriter.writerow(["ID","trial_id","task_order","task","outpos","serpos","recalled","stim","WTP"])

    for subj in data.keys():

        ID = subj

        trialKeys = data[subj].keys()
        trialids = []

        for tt in trialKeys:
            
            outSeq = [];
            inSeq = [];
            stim = []
            resp = []

            print subj
            print tt

            if data[subj][tt]["trial_type"]=="multi-typed":

                inSeq = data[subj][tt]["list"]
                ll = len(inSeq)

                initString = [ID, data[subj][tt]["trial_id"], data[subj][tt]["task"], data[subj][tt]["phase"]]

                if data[subj][tt]["phase"]=="recall":
                    
                    outSeq = data[subj][tt]["responses"]
                    outSeq = outSeq[1:-1]
                    outSeq = outSeq.split(",")
                    outSeq = [kk[1:-1] for kk in outSeq]

                    outpos = 1
                    recalled = []

                    # check to see if all items are in first one
                    if len(outSeq[0])>2:
                        outSeq = outSeq[0].split(' ');

                    for resp in outSeq:

                        if resp:
                            matched = 0;
                            stim = -1

                            try:
                                resp_int = int(resp)
                            except:
                                resp_int = 1000
                            
                            for stimi in range(ll):
                                stim = inSeq[stimi]
                                

                                sdist =  abs(stim-resp_int)
                                if (sdist==0) and (matched==0): # we have a match

                                    toWrite = initString[:]

                                    toWrite.append(outpos) # outpos
                                    toWrite.append(stimi+1) # serpos
                                    if stimi in recalled: # recalled
                                        toWrite.append(-2) # repetition (ignore)
                                    else:
                                        toWrite.append(1)

                                    toWrite.append(stim) # stim val filler
                                    toWrite.append(-1) # WTP filler

                                    csvwriter.writerow(toWrite)

                                    recalled.append(stimi)

                                    matched = 1;

                                    outpos = outpos + 1

                            if matched==0: # intrusion
                                toWrite = initString[:]
                                toWrite.append(outpos)
                                toWrite.append(100+outpos)
                                
                                if outpos>1:
                                    if resp in outSeq[0:(outpos-2)]:
                                        toWrite.append(-3)
                                    else:
                                        toWrite.append(-1)
                                else:
                                    toWrite.append(-1)

                                toWrite.append(resp_int) # the actual response
                                toWrite.append(-1)

                                csvwriter.writerow(toWrite)

                                recalled.append(100+outpos)

                                outpos = outpos + 1

                    missed = list(set(range(ll)) - set(recalled))

                    for stimi in missed:
                        toWrite = initString[:]
                        toWrite.append(outpos)
                        toWrite.append(stimi+1)
                        toWrite.append(0)

                        toWrite.append(inSeq[stimi])
                        toWrite.append(-1)

                        csvwriter.writerow(toWrite)

                        outpos = outpos + 1

                elif data[subj][tt]["phase"]=="offer":

                    for stimi in range(ll):
                        outSeq = data[subj][tt]["responses"]
                        outSeq = outSeq[2:-2]

                        print outSeq

                        try:
                            resp = int(outSeq)
                            print int(outSeq)
                        except:    
                            resp = "NA"

                        toWrite = initString[:]
                        toWrite.append(-1) # outpos
                        toWrite.append(stimi+1) # serpos
                        toWrite.append(-1) # recalled
                        toWrite.append(data[subj][tt]["list"][stimi]) #stim
                        toWrite.append(resp) # WTP

                        csvwriter.writerow(toWrite)

    test_file.close()


if __name__ == "__main__":
    sys.exit(main())