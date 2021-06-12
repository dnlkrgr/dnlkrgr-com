server {
    root /home/daniel/hakyll/_site;
    index index.html;

    listen 443 ssl http2 default_server; # managed by Certbot
    listen [::]:443 ssl http2 default_server; # managed by Certbot
    server_name dnlkrgr.com;
    
    location / { 
        try_files $uri $uri/ =404;

        # Rate Limiting
        limit_req zone=reqlimit burst=20; # Max burst of request
        limit_req_status 460; # Status to send
        # Connections Limiting
        limit_conn connlimit 20; # Number ofdownloads per IP        
        
        # Bandwidth Limiting
        limit_rate 4096k; # Speed limit (here is on kb/s)
    }   

    # location ~* \.(jpg|jpeg|png|gif|ico)$ {
    #    expires 30d;
    # }
    # location ~* \.(css|js)$ {
    #    expires 7d;
    # }

    ssl_certificate /etc/letsencrypt/live/dnlkrgr.io/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/dnlkrgr.io/privkey.pem; # managed by Certbot
    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot

}

server {
    listen 80 ;
    listen [::]:80 ;

    server_name dnlkrgr.com;

    return 301 https://$server_name$request_uri;
}

server {
    listen 80 ;
    listen [::]:80 ;
    listen 443 ssl http2 ;
    listen [::]:443 ssl http2;

    server_name www.dnlkrgr.com;

    return 301 https://dnlkrgr.com$request_uri;

    ssl_certificate /etc/letsencrypt/live/dnlkrgr.io/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/dnlkrgr.io/privkey.pem; # managed by Certbot
    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot
}
