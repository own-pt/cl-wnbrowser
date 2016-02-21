# script to split the UD_Portuguese CONLLU files into different classes

for f in VERB ADJ ADV NOUN; do
    cat *conllu | grep -v "^$" | grep -v "^#" | awk '{print $3 "\t" $4}' | grep $f | sort | uniq | awk '{print $1}' > pt-ud-$f.txt &
    cat *conllu | grep -v "^$" | grep -v "^#" | awk '{print $3 "\t" $4}' | grep $f | sort | uniq -c | sort -nr | awk '{print $1 "," $2}' > pt-ud-$f-freq.csv &
done

wait
