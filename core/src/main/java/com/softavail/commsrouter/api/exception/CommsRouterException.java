/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.exception;

/**
 *
 * @author ikrustev
 */
public class CommsRouterException extends Exception {

  public CommsRouterException() {
    super();
  }

  public CommsRouterException(String message) {
    super(message);
  }

  public CommsRouterException(Throwable cause) {
    super(cause);
  }

  public CommsRouterException(String message, Throwable cause) {
    super(message, cause);
  }

}
