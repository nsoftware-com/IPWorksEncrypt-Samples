/*
 * IPWorks Encrypt 2024 Java Edition - Sample Project
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

import java.io.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Base64;

import ipworksencrypt.Certificate;
import ipworksencrypt.EzRand;
import ipworksencrypt.IPWorksEncryptException;
import ipworksencrypt.JWS;

public class jws extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length == 4 || args.length == 5) {
			switch (args[0]) {
			case "sign":
				if (args.length == 5) {
					sign(args[1], args[2], args[3], args[4]);
				} else { // length == 4
					sign(args[1], args[2], args[3], ""); // no password
				}
				break;
			case "verify":
				verify(args[1], args[2], args[3]);
				break;
			default:
				displayHelp("First argument must be either 'sign' or 'verify'.");
			}
		} else {
			displayHelp(args.length + " arguments entered, 4 or 5 expected.");
		}

	}
	
	private static void sign(String alg, String key, String message, String password) {
		try {
			JWS jws = new JWS();

			// Set proper algorithm and key (using key from args[2] which must
			// be Base64 key for HMAC, or name of key file for RSA/ECDSA)
			switch (alg) {
			case "HS256":
				jws.setAlgorithm(JWS.jwsHS256);
				if(key.equals("0")) {
					key = generateBase64Key(alg);
				}
				jws.config("KeyEncoding=1"); //base64
				jws.setKey(key);
				break;
			case "HS384":
				jws.setAlgorithm(JWS.jwsHS384);
				if(key.equals("0")) {
					key = generateBase64Key(alg);
				}
				jws.config("KeyEncoding=1"); //base64
				jws.setKey(key);
				break;
			case "HS512":
				jws.setAlgorithm(JWS.jwsHS512);
				if(key.equals("0")) {
					key = generateBase64Key(alg);
				}
				jws.config("KeyEncoding=1"); //base64
				jws.setKey(key);
				break;
			case "RS256":
				jws.setAlgorithm(JWS.jwsRS256);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			case "RS384":
				jws.setAlgorithm(JWS.jwsRS384);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			case "RS512":
				jws.setAlgorithm(JWS.jwsRS512);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			case "PS256":
				jws.setAlgorithm(JWS.jwsPS256);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			case "PS384":
				jws.setAlgorithm(JWS.jwsPS384);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			case "PS512":
				jws.setAlgorithm(JWS.jwsPS512);
				jws.setCertificate(new Certificate(Certificate.cstPFXFile, key, password, "*"));
				break;
			default:
				displayHelp("Unsupported algorithm selected.");
				System.exit(0);
				break;
			}

			// Sign payload and display output
			jws.setInputMessage(message);
			jws.sign();
			String signed = new String(jws.getOutputMessage(), "UTF-8");
			System.out.print("Payload signed:\n" + signed);

		} catch (IPWorksEncryptException e) {
			System.err.println("Error [" + e.getCode() + "]: " + e.getMessage());
			displayHelp("");
		} catch (Exception e) {
			e.printStackTrace();
			displayHelp("");
		}
	}
	
	//generate bytes and convert to base64
	private static String generateBase64Key(String alg) {
		String base64Key = "";
		try {
			// Generate key
			EzRand ezrand = new EzRand();
			switch (alg) {
			case "HS256":
				ezrand.setRandBytesLength(32);
				break;
			case "HS384":
				ezrand.setRandBytesLength(48);
				break;
			case "HS512":
				ezrand.setRandBytesLength(64);
				break;
			default:
				displayHelp("Unsupported algorithm entered.");
				System.exit(0);
				break;
			}
			ezrand.getNextBytes();
			base64Key = new String(Base64.getEncoder().encode(ezrand.getRandBytes()), "UTF-8");
			System.out
					.println("Key generated for signing: " + base64Key);
			
		} catch (IPWorksEncryptException e) {
			System.err.println("Error [" + e.getCode() + "]: " + e.getMessage());
			displayHelp("");
		} catch (Exception e) {
			e.printStackTrace();
			displayHelp("");
		}
		return base64Key;
	}


	private static void verify(String alg, String key, String signed) {
		JWS jws = new JWS();
		try {
			switch (alg) {
			case "HS256":
			case "HS384":
			case "HS512":
				jws.config("KeyEncoding=1"); //base64
				jws.setKey(key);
				break;
			case "RS256":
			case "RS384":
			case "RS512":
			case "PS256":
			case "PS384":
			case "PS512":
				jws.setCertificate(new Certificate(key));
				break;
			default:
				displayHelp("Unsupported algorithm selected.");
				System.exit(0);
				break;

			}
			jws.setInputMessage(signed);
			jws.verify();
			String message = new String(jws.getOutputMessage(), "UTF-8");
			System.out.print("JWS string verified:\n" + message);
		} catch (IPWorksEncryptException e) {
			System.err.println("Error [" + e.getCode() + "]: " + e.getMessage());
			displayHelp("");
		} catch (Exception e) {
			e.printStackTrace();
			displayHelp("");
		}
	}

	private static void displayHelp(String message) {
		System.out.println("Invalid arguments entered. " + message);
		System.out.println("Usage: jws action algorithm key input [keyPassword]");
		System.out.println("\taction          the action to perform - 'sign' or 'verify'");
		System.out.println(
				"\talgorithm       the HMAC or RSA algorithm - 'HS256', 'HS384', 'HS512', 'RS256', 'RS384', 'RS512', 'PS256', 'PS384', or 'PS512'");
		System.out.println(
				"\tkey             HMAC - Base64 key or '0' to generate key; RSA - filename of key certificate (private cert for sign, public cert for verify)");
		System.out.println("\tinput           the payload string to sign or JWS string to verify");
		System.out.println(
				"\tkeyPassword     key certificate password if necessary (required only for private certificates with passwords)");
		System.out.println("Examples: jws sign HS256 txAVam2uGT20a+ZJC1VWVGCM8tFYSKyJlw+2fgS/BdA= \"Test message\"");
		System.out.println("          jws sign HS512 0 \"Test message\"");
		System.out.println("          jws sign RS384 .\\testrsapriv.pfx \"Test message\" test");
		System.out.println(
				"          jws verify HS256 ygIg4/Ut0KwUK2nS6fnflj1C5pAhgiXmVzqRqR2WTyU= eyJhbGciOiJIUzI1NiJ9.SGVsbG8.Deg4sWY8OL1pbXh6zVy7Wkr2brjVUrMBrIzeY5WlxM4");
		System.out.println(
				"          jws verify PS256 .\\testrsapub.cer eyJhbGciOiJQUzI1NiJ9.SGVsbG8.AqVXRmp7nmy74WQSoFrpY-Y4flb60n2e_XTjl51t0P1l-BqSCFj79wfaNf9-MJxCYbHkuFPjwkBq9-vvzxse0V-Bd0cjlXA9RY-LRn_wRHXRZUqParsZhsvWSqHY8MC4xAkXWCJuiDPWIuvDnd8mJDr_7vVbjIRipfifPkMMn3ePSvRSXWSBobalZxM320sYhReDgCZi5Mjb21cMSdowWj048AXFM86yL50UTh5rl2op3dG5JB9JbqBwVPDybdG7TK9r_84LYAajbTF7MepyMGWMAP7oSV1G-zBnBqpUC-HpTMRC-9xt9G3H0t1lUPePOBwB5ZdMeABrkFOSTwcIbQ");

	}
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksEncryptException) {
      System.out.print(" (" + ((IPWorksEncryptException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



