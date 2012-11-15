rename -v "s/\.csv/\.txt/g" *.csv
for ((i=1;i<107adsa;i++))
do
awk -v variable=${i}  '{ printf variable; printf ","; print }' s${i}.txt >> combine.txt
done
rename -v "s/\.txt/\.csv/g" *.txt
