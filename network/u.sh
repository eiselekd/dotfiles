
mkdir -p /var/lib/unifi/config
docker pull linuxserver/unifi 
docker create \
  --name=unifi \
  -v /var/lib/unifi/config:/config \
  -e PGID=1000 -e PUID=1000  \
  -p 3478:3478/udp \
  -p 10001:10001/udp \
  -p 8080:8080 \
  -p 8081:8081 \
  -p 8443:8443 \
  -p 8843:8843 \
  -p 8880:8880 \
  -p 6789:6789 \
  linuxserver/unifi
