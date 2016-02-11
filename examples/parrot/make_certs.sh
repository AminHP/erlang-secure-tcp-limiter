#! /bin/bash

openssl req -x509 -newkey rsa:2048 -keyout certs/key.pem -out certs/cert.pem -days XXX -nodes

