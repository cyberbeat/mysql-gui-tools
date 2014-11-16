
template=mysql-query-browser-template.po


tr -d \\r < $template > tmp
mv tmp $template

python ../../mysql-gui-common/tools/glade2po.py ../res/linux/*.glade --except-from=$template > glade.pot
msgcat -s -o new.pot glade.pot $template 
mv -f new.pot $template 
pofiles=`/bin/ls -1 *.po|grep -v template`

for po in $pofiles; do
	echo "Updating $po"
	msgmerge $po $template > new-$po
	mv -f new-$po $po
done
