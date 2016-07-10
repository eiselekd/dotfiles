. vpn.txt

apt-get install strongswan xl2tpd

bash vpn-cert.sh

#cp vpnca.cert.pem /usr/share/ca-certificates
#dpkg-reconfigure ca-certificates

cp vpnserver.key.pem /etc/ipsec.d/private/
cp vpnca.cert.pem /etc/ipsec.d/cacert/
cp vpnserver.cert.pem /etc/ipsec.d/cert/

echo <<EOF
conn setup
     virtual_private=%v4:192.168.0.0/16
     nat_traversal=yes

conn vpnconn
      type=transport
      left=<$serverip>
      leftcert=vpnserver.cert.der
      #leftprotoport=17/1701
      right=%any
      rightca="$cadn"
      #rightsubnet=192.168.43.0/24
      #rightprotoport=17/1701
      keyexchange=ikev2
      auto=start

#no leftsubnet on udp 17/1701

EOF

iptables --table nat --append POSTROUTING --jump MASQUERADE
echo 1 > /proc/sys/net/ipv4/ip_forward
