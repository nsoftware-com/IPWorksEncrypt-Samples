<?php
/*
 * IPWorks Encrypt 2024 PHP Edition - Sample Project
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
require_once('../include/ipworksencrypt_jws.php');
require_once('../include/ipworksencrypt_certmgr.php');
require_once('../include/ipworksencrypt_ezrand.php');
require_once('../include/ipworksencrypt_const.php');
?>
<?php

function GenerateBase64Key($algorithm) {
  $base64key = "";
  try
  {
    // Generate key.
    $ezrand = new IPWorksEncrypt_Ezrand();
    switch ($algorithm)
    {
      case "hs256":
        $ezrand->setRandBytesLength(32);
        break;
      case "hs384":
        $ezrand->setRandBytesLength(48);
        break;
      case "hs512":
        $ezrand->setRandBytesLength(64);
        break;
      default:
        echo "Invalid algorithm selection.\n";
        return;
    }
    $ezrand->doConfig("OutputEncoding=1"); // base64
    $ezrand->doGetNextBytes();
    $base64key = str_replace("\r\n", "", $ezrand->getRandBytes());
    echo "Key generated for signing: " . $base64key . "\n";
  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
  return $base64key;
}

if ($argc < 9) {
  echo "usage: php jws.php -a action -alg algorithm -k key -i input [-p keypassword]\n\n";
  echo "  -a    action       chosen from {sign, verify}\n";
  echo "  -alg  algorithm    the HMAC or RSA algorithm to use, chosen from {HS256, HS384, HS512, RS256, RS384, RS512, PS256, PS384, PS512}\n";
  echo "  -k    key          for HMAC, the base64 key or '0' to generate a key\n";
  echo "                     for RSA, the path to the key certificate (private for signing, public for verifying)\n";
  echo "  -i    input        the payload string to sign or JWS string to verify\n";
  echo "  -p    keypassword  the key certificate password (required only for private certificates with passwords)\n\n";
  echo "Examples: php jws.php -a sign -alg HS256 -k txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= -i \"Test message\"\n";
  echo "          php jws.php -a sign -alg HS512 -k 0 -i \"Test message\"\n";
  echo "          php jws.php -a sign -alg RS384 -k .\\testrsapriv.pfx -i \"Test message\" -p test\n";
  echo "          php jws.php -a verify -alg HS256 -k ygIg4/Ut0KwUK2nS6fnflj1C5pAhgiXmVzqRqR2WTyU= -i eyJhbGciOiJIUzI1NiJ9.SGVsbG8.Deg4sWY8OL1pbXh6zVy7Wkr2brjVUrMBrIzeY5WlxM4\n";
  echo "          php jws.php -a verify -alg PS256 -k .\\testrsapub.cer -i eyJhbGciOiJQUzI1NiJ9.SGVsbG8.AqVXRmp7nmy74WQSoFrpY-Y4flb60n2e_XTjl51t0P1l-BqSCFj79wfaNf9-MJxCYbHkuFPjwkBq9-vvzxse0V-Bd0cjlXA9RY-LRn_wRHXRZUqParsZhsvWSqHY8MC4xAkXWCJuiDPWIuvDnd8mJDr_7vVbjIRipfifPkMMn3ePSvRSXWSBobalZxM320sYhReDgCZi5Mjb21cMSdowWj048AXFM86yL50UTh5rl2op3dG5JB9JbqBwVPDybdG7TK9r_84LYAajbTF7MepyMGWMAP7oSV1G-zBnBqpUC-HpTMRC-9xt9G3H0t1lUPePOBwB5ZdMeABrkFOSTwcIbQ\n\n";
  return;
}

try {
  $jws = new IPWorksEncrypt_JWS();
  $action = $algorithm = $key = $keyPassword = $input = "";

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-alg") {
        $algorithm = strtolower($argv[$i + 1]);
      }
      if ($argv[$i] == "-a") {
        $action = $argv[$i + 1];
      }
      if ($argv[$i] == "-k") {
        $key = $argv[$i + 1];
      }
      if ($argv[$i] == "-i") {
        $input = $argv[$i + 1];
      }
      if ($argv[$i] == "-p") {
        $keyPassword = $argv[$i + 1];
      }
    }
  }

  if ($action == "sign") {
    // sign
    switch ($algorithm) {
      case "hs256":
        $jws->setAlgorithm(0);
        // todo
        if ($key == "0") { $key = GenerateBase64Key($algorithm); }
        $jws->doConfig("KeyEncoding=1"); // base64
        $jws->setKey($key);
        break;
      case "hs384":
        $jws->setAlgorithm(1);
        // todo
        if ($key == "0") { $key = GenerateBase64Key($algorithm); }
        $jws->doConfig("KeyEncoding=1"); // base64
        $jws->setKey($key);
        break;
      case "hs512":
        $jws->setAlgorithm(2);
        // todo
        if ($key == "0") { $key = GenerateBase64Key($algorithm); }
        $jws->doConfig("KeyEncoding=1"); // base64
        $jws->setKey($key);
        break;
      case "rs256":
        $jws->setAlgorithm(3);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      case "rs384":
        $jws->setAlgorithm(4);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      case "rs512":
        $jws->setAlgorithm(5);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      case "ps256":
        $jws->setAlgorithm(6);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      case "ps384":
        $jws->setAlgorithm(7);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      case "ps512":
        $jws->setAlgorithm(8);
        $jws->setCertStoreType(2); //PFX File
        $jws->setCertStore($key);
        $jws->setCertStorePassword($keyPassword);
        $jws->setCertSubject("*");
        break;
      default:
        echo "Invalid algorithm selection.\n";
        return;
    }
    $jws->setInputMessage($input);
    $jws->doSign();
    echo "Payload signed:\n" . $jws->getOutputMessage() . "\n";
  } elseif ($action == "verify") {
    // verify
    switch ($algorithm) {
      case "hs256":
      case "hs384":
      case "hs512":
        $jws->doConfig("KeyEncoding=1");
        $jws->setKey($key);
        break;
      case "rs256":
      case "rs384":
      case "rs512":
      case "ps256":
      case "ps384":
      case "ps512":
        $jws->setCertStoreType(99);
        $jws->setCertStore($key);
        $jws->setCertSubject("*");
        break;
      default:
        throw new Exception("Invalid algorithm selection.\n");
    }
    // Verify JWS string and display output.
    $jws->setInputMessage($input);
    $jws->doVerify();
    echo "JWS string verified:\n" . $jws->getOutputMessage() . "\n";
  } else {
    echo "Invalid action.\n";
  }
}  catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>