. vpn.txt

apt-get install strongswan xl2tpd

bash vpn-cert.sh

#cp vpnca.cert.pem /usr/share/ca-certificates
#dpkg-reconfigure ca-certificates

cp vpnserver.key.pem /etc/ipsec.d/private/
cp vpnca.cert.pem /etc/ipsec.d/cacert/
cp vpnserver.cert.pem /etc/ipsec.d/cert/

echo <<EOF

conn vpnconn
      left=
      #left=%defaultroute
      leftsubnet=192.168.0.0/16
      leftcert=vpnserver.cert.der
      right=%any
      rightca="$cadn"
      keyexchange=ikev2
      auto=start

EOF
