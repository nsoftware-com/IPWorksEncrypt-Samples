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
import ipworksencrypt.*;

public class encrypt extends ConsoleDemo {

  public static void main(String[] args) {
    

    try {
      String algorithm = "";
      String inputFile = "";
      String outputFile = "";
      String operation = "";
      String password = "";
      EzCrypt ezcrypt;
      
      System.out.println("******************************************************************************");
      System.out.println("* This demo show how to encrypt and decrypt files using symmetric algorithms *");
      System.out.println("* like AES, DES, 3DES, etc. The components can also be used to encrypt and   *");
      System.out.println("* decrypt in memory or to and from streams.                                  *");
      System.out.println("******************************************************************************");
    	
      
      System.out.println("Specify an algorithm:");
      System.out.println("1) AES");
      System.out.println("2) CAST");
      System.out.println("3) DES");
      System.out.println("4) IDEA");
      System.out.println("5) RC2");
      System.out.println("6) RC4");
      System.out.println("7) 3DES");
      System.out.println("8) Blowfish");
      System.out.println("9) Twofish");
      
      algorithm = prompt("\nAlgorithm",":","1");
      
      System.out.println("\nSelect an operation:");
      System.out.println("1) Encrypt");
      System.out.println("2) Decrypt");
      
      operation = prompt("\nOperation",":","1");
      
      //A key password is used to generate the Key and IV. 
      //When using a key password you only need to save this value. 
      //This makes key management much simpler.
      password = prompt("\nPassword");
      
      inputFile = prompt("Input File");
      outputFile = prompt("Output File");
      
      prompt("\nPress enter to begin the operation.\n","");
      
	  ezcrypt = new EzCrypt();
      switch(Integer.parseInt(algorithm))
      {
    	case 1:
    	  ezcrypt.setAlgorithm(EzCrypt.ezAES);
    	  break;
        case 2:
    	  ezcrypt.setAlgorithm(EzCrypt.ezCAST);
    	  break;
    	case 3: 
    	  ezcrypt.setAlgorithm(EzCrypt.ezDES);
    	  break;
    	case 4:
    	  ezcrypt.setAlgorithm(EzCrypt.ezIDEA);
    	  break;
    	case 5:
    	  ezcrypt.setAlgorithm(EzCrypt.ezRC2);
    	  break;
    	case 6:
    	  ezcrypt.setAlgorithm(EzCrypt.ezRC4);
    	  break;
    	case 7:
    	  ezcrypt.setAlgorithm(EzCrypt.ezTripleDES);
    	  break;
    	case 8:
    	  ezcrypt.setAlgorithm(EzCrypt.ezBlowfish);
    	  break;
    	case 9:
    	  ezcrypt.setAlgorithm(EzCrypt.ezTwofish);
    	  break;
    	default:
    	  System.out.println("Invalid algorithm.");
    	  break;
      }
	  
      if(operation.equals("1")) //Encrypt
      {
    	ezcrypt.setInputFile(inputFile);
    	ezcrypt.setOutputFile(outputFile);
    	ezcrypt.setKeyPassword(password);
    	ezcrypt.encrypt();
    	  
    	System.out.println("Encryption successful!");
      }
      else if(operation.equals("2")) //Decrypt
      {
    	ezcrypt.setInputFile(inputFile);
    	ezcrypt.setOutputFile(outputFile);
    	ezcrypt.setKeyPassword(password);
    	ezcrypt.decrypt();
    	  
    	System.out.println("Decryption successful!");    	  
      }
      else
      {
    	  System.out.println("Invalid operation specified.");
      }
      
   
    } catch (IPWorksEncryptException ex) {
      System.out.println("IPWorksEncryptException exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
    } catch (Exception ex) {
      System.out.println(ex.getMessage());
    }
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



