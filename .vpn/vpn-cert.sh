set -x
rm *der

. vpn.txt

ipsec pki --gen -s 4096 > vpnca.key.der
ipsec pki --self --ca --lifetime 1460 --in vpnca.key.der --dn "${cadn}" > vpnca.cert.der
ipsec pki --print --in vpnca.cert.der 

echo "-----------------------------------"
ipsec pki --gen > vpnserver.key.der
ipsec pki --pub --in vpnserver.key.der | ipsec pki --issue --lifetime 730  --cacert vpnca.cert.der --cakey vpnca.key.der --dn "${serverdn}" --san "${serversan0}" --san "${serversan1}" --crl "${servercrl}" > vpnserver.cert.der
ipsec pki --print --in vpnserver.cert.der | tee vpnserver.cert.txt

echo "-----------------------------------"
ipsec pki --gen > vpnclient.key.der
ipsec pki --pub --in vpnclient.key.der | ipsec pki --issue --lifetime 730  --cacert vpnca.cert.der --cakey vpnca.key.der --dn "${clientdn}" --san "${clientsan0}" --san "${clientsan1}" --crl "${clientcrl}" > vpnclient.cert.der
ipsec pki --print --in vpnclient.cert.der | tee vpnclient.cert.txt


echo "+++++++++++++++++++++++++++++++++++"
openssl rsa -inform DER -in vpnca.key.der -outform PEM -out vpnca.key.pem 
openssl rsa -inform DER -in vpnserver.key.der -outform PEM -out vpnserver.key.pem 
openssl rsa -inform DER -in vpnclient.key.der -outform PEM -out vpnclient.key.pem 

openssl x509 -inform DER -in vpnca.cert.der -outform PEM -out vpnca.cert.pem
openssl x509 -inform DER -in vpnserver.cert.der -outform PEM -out vpnserver.cert.pem
openssl x509 -inform DER -in vpnclient.cert.der -outform PEM -out vpnclient.cert.pem
