package org.squeryl.internals


object Utils {

  /**
   * Will attempt to evaluate a string expression and will catch any exception.
   * For use in circumstances when loggin is needed (i.e. a fatal error has already occured
   * and we need to log as much info as possible (i.e. put as much info as possible in the 'black box').
   * Also used to allow dumping (ex. for logging) a Query AST *before* it is completely built.
   */
  def failSafeString(s: =>String) =
    try {
      s
    }
    catch {
      case e:Exception => "cannot evaluate" 
    }
}