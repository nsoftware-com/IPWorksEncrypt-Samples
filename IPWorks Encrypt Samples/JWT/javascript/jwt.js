/*
 * IPWorks Encrypt 2024 JavaScript Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks Encrypt in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksencrypt
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */
 
const readline = require("readline");
const ipworksencrypt = require("@nsoftware/ipworksencrypt");

if(!ipworksencrypt) {
  console.error("Cannot find ipworksencrypt.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

'use strict';
// tslint:disable no-console no-var-keyword
import * as ipworksencrypt from "@nsoftware/ipworksencrypt";// import the public Product wrappers

const key_256 = "txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=";
const key_384 = "5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2";
const key_512 = "AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==";
const password = "test";

main();

async function main() {

  //Usage
  if (process.argv.length !== 9 && process.argv.length !== 10) {
    console.log("Usage:\n\tnode .\\jwt.js create algorithm <alg> key <key> kid <keyid> ['<config>']")
    console.log("\tnode .\\jwt.js validate algorithm <alg> key <key> jwt <encodedjwt>")
    console.log("Options:\n\t<alg>\t\tSigning Algorithm - hs256, hs384, hs512, rs256, rs384, rs512, ps256, ps384, or ps512")
    console.log("\t<key>\t\tHMAC key or a string path to public/private key. '0' to use hardcoded HMAC keys.")
    console.log("\t<config>\tString of claim name=value pairs separated by commas - \"aud=test,sub=test,iss=test,jti=test,iat=test\"")
    console.log("Examples:\n\tnode .\\jwt.js create algorithm hs256 key txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= kid test")
    console.log("\tnode .\\jwt.js create algorithm rs512 key .\\testrsapriv.pfx kid test")
    console.log("\tnode .\\jwt.js create algorithm hs512 key 0 keyid test \"aud=test,sub=test,iss=test,jti=test,iat=1234\"")
    console.log("\tnode .\\jwt.js validate algorithm hs256 key 0 jwt eyJhbGciOiJIUzI1NiIsImtpZCI6IjAxMjM0NTY3ODkifQ.eyJpYXQiOjEyMzQnLCJpc3MiOiJ0ZXN0IiwianRpIjoidGVzdCIsInN1YiI6InRlc3QifQ.1GMSe_JMv0bOgHK1GyekW48wL6ZnFfCFN2pNg_OuOaE")
    console.log("\tnode .\\jwt.js validate algorithm rs384 key .\\testrsapub.cer jwt eyJhbGciOiJSUzM4NCIsImtpZCI6IjAxMjM0NTY3OTg5In0.eyJhdWQiOlsiYXVkaWVuY2UiXSwiZXhwIjowNDAxMjAxNywiaXNzIjoiaXNzdWVyIiwic3ViIjoic3ViamVjdCJ9.Nfb7tYKLDeAb_2wfJ134Tq58_B3KZYb2Lu6xytGyBlnnLizbY5lQQBQdNrKt0qnhjNEdtYeUzF1ZJH_s_XsbiOd3qgxkdfALMJbB83_xPGK9TBvou-XfgCRPvib6fiLwQPf6W5n3T0G3RiMPWyUR5DrluwHo3JcyzFUnXv1wy8hyYePJJYbmOK5TJv_c0kOp5ObsB4kCCDSecFBn0tbUZ3p6UY4wz48ijivoq8Mzqbp0voaIP6I3k41lUERA3SntAG4Jtzx1D56rmcR3tWv_9uqWA5W1Msp9g1Yyf-6PcOdKn09xf_BNAnT8ag4NcBXBd1_3K-p7kT1Bhm4uZNxWWA")
    process.exit();
  }

  const jwt = new ipworksencrypt.jwt();

  if (process.argv[2] === "create") {
    await jwt.reset();

    await parseAlg(process.argv[4], process.argv[6], jwt);

    jwt.setKeyId(process.argv[8]);

    if (process.argv.length === 10) {
      const config_arr = process.argv[9].split(",");
      for (let i = 0; i < config_arr.length; i++) {
        const term = config_arr[i].split("=");

        switch (term[0]) {
          case "aud":
            jwt.setClaimAudience(term[1]);
            break;
          case "sub":
            jwt.setClaimSubject(term[1]);
            break;
          case "iss":
            jwt.setClaimIssuer(term[1]);
            break;
          case "jti":
            jwt.setClaimJWTId(term[1]);
            break;
          case "iat":
            jwt.setClaimIssuedAt(term[1]);
            break;
          default:
            console.log(`Invalid claim type passed in config: ${term[0]}`);
            break;

        }
      }
    }
    await jwt.sign();
    console.log("Encoded JWT: " + jwt.getEncodedJWT());

  } else if (process.argv[2] === "validate") {
    await jwt.reset();
    await setSA(process.argv[4], process.argv[6], jwt);
    jwt.on("SignerInfo", (e) => (console.log(`alg=${e.algorithm}, kid=${e.keyId}`)))
      .on("ClaimInfo", (e) => (console.log(`${e.name}=${e.value}`)));

    jwt.setEncodedJWT(process.argv[8]);

    await jwt.verify();
  }
  process.exit(0);
}

async function setSA(alg, key, token) {
  switch (alg) {
    case "hs256":
      token.setSigningAlgorithm(0);
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_256;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      break;
    case "hs384":
      token.setSigningAlgorithm(1);
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_384;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      break;
    case "hs512":
      token.setSigningAlgorithm(2);
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_512;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      break;
    case "rs256":
      token.setSigningAlgorithm(3);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));
      break;
    case "rs384":
      token.setSigningAlgorithm(4);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));
      break;
    case "rs512":
      token.setSigningAlgorithm(5);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));
      break;
    case "es256":
      token.setSigningAlgorithm(6);
      token.setSignerCert(new ipworksencrypt.Certificate(8, key, "", "*"));
      break;
    case "es384":
      token.setSigningAlgorithm(7);
      token.setSignerCert(new ipworksencrypt.Certificate(8, key, "", "*"));

      break;
    case "es512":
      token.setSigningAlgorithm(8);
      token.setSignerCert(new ipworksencrypt.Certificate(8, key, "", "*"));

      break;
    case "ps256":
      token.setSigningAlgorithm(9);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));

      break;
    case "ps384":
      token.setSigningAlgorithm(10);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));

      break;
    case "ps512":
      token.setSigningAlgorithm(11);
      token.setSignerCert(new ipworksencrypt.Certificate(6, key, password, "*"));
      break;
    case "none":
      token.setSigningAlgorithm(12);
      break;
    default:
      break;
  }
}

async function parseAlg(alg, key, token) {
  switch (alg) {
    case "hs256":
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_256;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      token.setSigningAlgorithm(0);
      break;
    case "hs384":
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_384;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      token.setSigningAlgorithm(1);
      break;
    case "hs512":
      token.config("KeyEncoding=1"); //Base64
      if (key === "0") {
        key = key_512;
      }
      token.setKey(key, (e) => { if (e) { console.log(`${e.code} ${e.message}`); } });
      token.setSigningAlgorithm(2);
      break;
    case "rs256":
      token.setSigningAlgorithm(3);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));
      break;
    case "rs384":
      token.setSigningAlgorithm(4);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));
      break;
    case "rs512":
      token.setSigningAlgorithm(5);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));
      break;
    case "es256":
      token.setSigningAlgorithm(6);
      token.setCertificate(new ipworksencrypt.Certificate(6, key, "", "*"));
      break;
    case "es384":
      token.setSigningAlgorithm(7);
      token.setCertificate(new ipworksencrypt.Certificate(6, key, "", "*"));

      break;
    case "es512":
      token.setSigningAlgorithm(8);
      token.setCertificate(new ipworksencrypt.Certificate(6, key, "", "*"));

      break;
    case "ps256":
      token.setSigningAlgorithm(9);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));

      break;
    case "ps384":
      token.setSigningAlgorithm(10);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));

      break;
    case "ps512":
      token.setSigningAlgorithm(11);
      token.setCertificate(new ipworksencrypt.Certificate(2, key, password, "*"));

      break;
    case "none":
      token.setSigningAlgorithm(12);
      break;
    default:
      break;
  }
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
