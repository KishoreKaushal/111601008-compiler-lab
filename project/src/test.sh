for file in test/*.mc; do echo -e "Testing $file"; ./source <"$file"; echo -e "\r"; done;
