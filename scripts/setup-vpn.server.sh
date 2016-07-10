apt-get install strongswan xl2tpd

bash vpn-cert.sh

cp rootca.cert.pem /usr/share/ca-certificates
dpkg-reconfigure ca-certificates



