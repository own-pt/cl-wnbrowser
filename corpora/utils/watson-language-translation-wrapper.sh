while read p; do
    en=`curl -s -u "${wtuser}":"${wtpassword}" -X POST -F "text=$p" -F "source=pt" -F "target=en" "https://gateway.watsonplatform.net/language-translation/api/v2/translate"`
    echo $p, $en
done < $1
