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
      type=transport
      leftcert=vpnclient.cert.der
      #leftsubnet=192.168.0.0/24
      #leftprotoport=17/1701
      right=$serverip
      rightsubnet=192.168.0.0/16
      #rightprotoport=17/1701
      rightid="$serverdn"
      auto=start

#no leftsubnet on udp 17/1701
EOF

#ppp0      Link encap:Point-to-Point Protocol
#inet addr:192.168.0.2  P-t-P:192.168.0.107  Mask:255.255.255.255
#ip route add 192.168.0.101 via 192.168.0.107 dev ppp0
