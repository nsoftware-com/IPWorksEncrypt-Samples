/*
 * IPWorks Encrypt 2024 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include "../../include/ipworksencrypt.h"
#pragma comment(lib,"Crypt32.lib")

void displayHelp(char *message, char *arg0) {
	printf("Invalid arguments entered. ");
	printf(message);
	printf("\nUsage: .\\jwt.exe <action> <algorithm> <key> <input> [keyPassword]");
	printf("\n\taction          the action to perform - 'sign' or 'verify'");
	printf(
		"\n\talgorithm       the HMAC or RSA algorithm - 'HS256', 'HS384', 'HS512', 'RS256', 'RS384', 'RS512', 'PS256', 'PS384', or 'PS512'");
	printf(
		"\n\tkey             HMAC - Base64 key or '0' to generate key; RSA - filename of key certificate (private cert for sign, public cert for verify)");
	printf("\n\tinput           signing - a commma-delimited list of name=value pairs to specify headers and claim. possible names - 'kid', 'aud', 'sub', 'iss', 'jti', 'iat' or\n\t\t\tverifying - the encoded JWT.");
	printf(
		"\n\tkeyPassword     key certificate password if necessary (required only for private certificates with passwords)");
	printf("\nExamples: .\\jwt.exe sign HS256 txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= \"kid=1234,aud=test\"");
	printf("\n          .\\jwt.exe sign HS512 0 \"kid=1234,sub=test,aud=test\"");
	printf("\n          .\\jwt.exe sign RS384 .\\testrsapriv.pfx \"kid=1234\" test");
	printf("\n          .\\jwt.exe sign ES256 .\\testeccpriv.txt \"kid=1234,sub=test,aud=test\"");
	printf(
		"\n          .\\jwt.exe verify HS256 0 eyJhbGciOiJIUzI1NiIsImtpZCI6IjEyMyJ9.eyJhdWQiOlsidGVzdCJdLCJpYXQiOjE2MDAsImlzcyI6InRlc3QiLCJqdGkiOiJ0ZXN0Iiwic3ViIjoidGVzdCJ9.Vb_iZbUzAcwgEIaA4O0Kl2LTYPfmExURZuCip73ZOKY");
	printf(
		"\n          .\\jwt.exe verify PS256 .\\testrsapub.cer eyJhbGciOiJQUzI1NiIsImtpZCI6IjEyMyJ9.eyJhdWQiOlsidGVzdCJdLCJpYXQiOjE2MDAsImlzcyI6InRlc3QiLCJqdGkiOiJ0ZXN0Iiwic3ViIjoidGVzdCJ9.ACrMmHIfWuU35G_wcGEhd5RSV1dk2r5IqX2tccc-xzwKxJvZ6uAGHoGP7PchX9GVNNLHJGyvZO0tfvOCaQWEjgeET5GsjfvE39sOd8-UycMGDkwAPMnrRjbX3PhQZCp6UZuDxEY_27lqkCdrmDK4o1wRrsbk4fPK6-A8so8i9H_QWUOYhWToWg6jCLHt5X0mpWgrD5Yd_Ys49GlS8bf6g0N0tiVJPlXObO5-xZIXYaCZmiQHmPh4PY4AB5hXbwet_VF-psc0GnJdv1Ajl9ZoNzVCKwNhE1aFCeCGW66RymeNpQt5f2u4uyn1MF9fBwi1mjH6l8RFTMtGHpM_9u7TqA");
	printf("\n          .\\jwt.exe verify ES512 .\\testeccpub.txt eyJhbGciOiJFUzUxMiIsImtpZCI6IjEyMyJ9.eyJhdWQiOlsidGVzdCJdLCJpYXQiOjE2MDAsImlzcyI6InRlc3QiLCJqdGkiOiJ0ZXN0Iiwic3ViIjoidGVzdCJ9.fyGhNL0f6pTzkXOmibyqigHrZPkIeCJPrE6urATW2WsLL9Z2J_Cc-H1gVg0kUwv0TqY8McPfH_r16-lg1zoMKw");
}


class MyJWT : public JWT
{
public:

  MyJWT()
  {

  }

  virtual int FireHeaderParam(JWTHeaderParamEventParams *e)
  {
    printf("Header: %s=%s\n", e->Name, e->Value);
    return 0;
  }

  virtual int FireClaimInfo(JWTClaimInfoEventParams *e)
  {
    printf("Claim: %s=%s\n", e->Name, e->Value);
    return 0;
  }

  virtual int FireRecipientInfo(JWTRecipientInfoEventParams *e)
  {
    return 0;
  }

  virtual int FireError(JWTErrorEventParams *e)
  {
    return 0;
  }
};

int parseHeadersAndClaim(MyJWT *jwt, char *input) {
  char *token;

  token = strtok(input, ",="); // contains first name

  while (token != NULL) {    
    if (strcmp(token, "kid") == 0) {
      jwt->SetKeyId(strtok(NULL, ",=")); 
      token = strtok(NULL, ",="); // moves forward strtok to next name
    }
    else if (strcmp(token, "aud") == 0) {
      jwt->SetClaimAudience(strtok(NULL, ",="));
      token = strtok(NULL, ",=");
    }
    else if (strcmp(token, "sub") == 0) {
      jwt->SetClaimSubject(strtok(NULL, ",="));
      token = strtok(NULL, ",=");
    }
    else if (strcmp(token, "iss") == 0) {
      jwt->SetClaimIssuer(strtok(NULL, ",="));
      token = strtok(NULL, ",=");
    }
    else if (strcmp(token, "jti") == 0) {
      jwt->SetClaimJWTId(strtok(NULL, ",="));
      token = strtok(NULL, ",=");
    }
    else if (strcmp(token, "iat") == 0) {
      jwt->SetClaimIssuedAt(strtok(NULL, ",="));
      token = strtok(NULL, ",=");
    }
    else {
      return -1;
    }
    //token = strtok(NULL, ",=");
  }
  return 0;
}

int sign(char *alg, char *key, char *message, char *password, MyJWT *jwt) {
  jwt->Reset();
	int ret_code = 0;
	if (strcmp(alg, "HS256")==0) {
		jwt->SetSigningAlgorithm(0);
		if (strcmp(key, "0") == 0) {
			jwt->SetKey("txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=", 32);
		}
		else {
			jwt->Config("KeyEncoding=1"); //Base64 input
			jwt->SetKey(key, strlen(key));
		}		
	} else if (strcmp(alg, "HS384")==0) {
    jwt->SetSigningAlgorithm(1);
    if (strcmp(key, "0") == 0) {
			jwt->SetKey("5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2", 48);
		}
		else {
			jwt->Config("KeyEncoding=1"); //Base64 input
			jwt->SetKey(key, strlen(key));
		}
	}
	else if(strcmp(alg, "HS512")==0) {
    jwt->SetSigningAlgorithm(2);
    if (strcmp(key, "0") == 0) {
			jwt->SetKey("AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==", 64);
		}
		else {
			jwt->Config("KeyEncoding=1");
			jwt->SetKey(key, strlen(key));
		}
	}
	else if (strcmp(alg, "RS256")==0) {
    jwt->SetSigningAlgorithm(3);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "RS384")==0) {
    jwt->SetSigningAlgorithm(4);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "RS512")==0) {
    jwt->SetSigningAlgorithm(5);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "PS256")==0) {
    jwt->SetSigningAlgorithm(9);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "PS384")==0) {
    jwt->SetSigningAlgorithm(10);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "PS512")==0) {
    jwt->SetSigningAlgorithm(11);
    jwt->SetCertStoreType(CST_PFXFILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "ES256")==0) {
    jwt->SetSigningAlgorithm(6);
    jwt->SetCertStoreType(CST_PEMKEY_FILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "ES384")==0) {
    jwt->SetSigningAlgorithm(7);
    jwt->SetCertStoreType(CST_PEMKEY_FILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else if (strcmp(alg, "ES512")==0) {
    jwt->SetSigningAlgorithm(8);
    jwt->SetCertStoreType(CST_PEMKEY_FILE);
		jwt->SetCertStore(key, strlen(key));
		jwt->SetCertStorePassword(password);
		ret_code = jwt->SetCertSubject("*");
	}
	else {
		printf("Unsupported algorithm inputted.");
		return 1;
	}
	if (ret_code) {
		return ret_code;
	}
  parseHeadersAndClaim(jwt, message);
  ret_code = jwt->Sign();
  if (ret_code) {
	  return ret_code;
  }
	printf("Encoded JWT: %s\n", jwt->GetEncodedJWT());
	return ret_code;
}

int verify(char *alg, char *key, char *payload, MyJWT *jwt) {
  jwt->Reset();
	int ret_code = 0;
	if (strcmp(alg, "HS256") == 0) {
    if (strcmp(key, "0") == 0) {
      ret_code = jwt->SetKey("txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA=", 32);
    }
    else {
      jwt->Config("KeyEncoding=1");
      ret_code = jwt->SetKey(key, strlen(key));
    }
	}
  else if (strcmp(alg, "HS384") == 0 ) {
    if (strcmp(key, "0") == 0) {     
      ret_code = jwt->SetKey("5C/iq/SVHc1i++8elF0u3Cg8w1D1Nj8Idrsw2zzIQeLrolmPk5d26f6MxTE3Npy2", 48);
    }
    else {
      jwt->Config("KeyEncoding=1");
      ret_code = jwt->SetKey(key, strlen(key));
    }
  }
  else if (strcmp(alg, "HS512") == 0) {
    if (strcmp(key, "0") == 0) {
      ret_code = jwt->SetKey("AGVJSwvgVMU0cspZ7ChlxURcgCcdj7QV6nm0fr0C/rNtuh8F5uA7nCs4efKuWUDBw7/s9ikfTm0Kx4uZ3SYXcA==", 64);
    }
    else {
      jwt->Config("KeyEncoding=1");
      ret_code = jwt->SetKey(key, strlen(key));
    }
  }
	else if (strcmp(alg, "RS256") == 0 || strcmp(alg, "RS384") == 0 || strcmp(alg, "RS512") == 0 || strcmp(alg, "PS256") == 0 || strcmp(alg, "PS384") == 0 || strcmp(alg, "PS512") == 0) {
		jwt->SetSignerCertStoreType(CST_PEMKEY_FILE);
		jwt->SetSignerCertStore(key, strlen(key));
		ret_code = jwt->SetSignerCertSubject("*");
	}
	else if (strcmp(alg, "ES256") == 0 || strcmp(alg, "ES384") == 0 || strcmp(alg, "ES512") == 0) {
		jwt->SetSignerCertStoreType(CST_PUBLIC_KEY_FILE);
		jwt->SetSignerCertStore(key, strlen(key));
		ret_code = jwt->SetSignerCertSubject("*");
	}
	else {
		printf("Unsupported algorithm inputted.");
		return 1;
	}
  if (ret_code) {
    return ret_code;
  }
	jwt->SetEncodedJWT(payload);
	ret_code = jwt->Verify();
  return ret_code;
}

int main(int argc, char **argv)
{
  MyJWT jwtobj;
  MyJWT *jwt = &jwtobj;

	int ret_code = 0;

  
	if (argc == 5 || argc == 6) {
		if (strcmp(argv[1],"sign")==0) {
			if (argc == 6) {
				ret_code = sign(argv[2], argv[3], argv[4], argv[5], jwt);
			}
			else { // length == 5
				 ret_code = sign(argv[2], argv[3], argv[4], "", jwt); // no password
			}
		}
		else if (strcmp(argv[1], "verify")==0) {
			ret_code = verify(argv[2], argv[3], argv[4], jwt);
		}
		else {
			displayHelp("First argument must be either 'sign' or 'verify'.", argv[0]);
		}
	}
	else {
		displayHelp("5 or 6 arguments expected.", argv[0]);
	}
	if (ret_code) {
		printf("Error: %d", ret_code);
	}
	return ret_code;
}













