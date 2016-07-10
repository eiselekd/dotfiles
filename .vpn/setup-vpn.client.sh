. vpn.txt

apt-get install strongswan xl2tpd

#cp vpnca.cert.pem /usr/share/ca-certificates
#dpkg-reconfigure ca-certificates

cp vpnclient.key.pem /etc/ipsec.d/private/
cp vpnca.cert.pem /etc/ipsec.d/cacert/
cp vpnclient.cert.pem /etc/ipsec.d/cert/

echo <<EOF
conn setup
     virtual_private=%v4:192.168.0.0/16
     nat_traversal=yes

conn vpnconn
      leftcert=vpnclient.cert.der
      leftsubnet=192.168.0.0/24
      right=$serverip
      rightsubnet=192.168.0.0/16
      rightid="$serverdn"
      auto=start

EOF

