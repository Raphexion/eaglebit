for ii in $(seq 1 20)
do
  id=$(head /dev/urandom | tr -dc a-z | head -c 4 ; echo '')
  echo $id

  openssl genrsa -out ${id}.private.pem 2048
  openssl rsa -in ${id}.private.pem -out ${id}.public.pem -outform PEM -pubout

done
