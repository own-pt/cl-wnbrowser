echo "<html><body><table>"
cat pertains-sumo-locations.tsv | awk '{printf ("<tr><td><a href=\"http://wnpt.brlcloud.com/wn/synset?id=%s\">%s</a></td><td>%s</td><td>%s</td><td>%s</td><td><a href=\"http://wnpt.brlcloud.com/wn/synset?id=%s\">%s</a></td><td>%s</td><td>%s</td><td>%s</td></tr>", $1, $1, $2, $3, $4, $5, $5, $6, $7, $8)}'
echo "</table></body></html>"
