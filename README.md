XML/HTML parsers
================

Table of contents
-----------------

   * Description
   * Authors
   * Documentation
   * Compatibility
   * Documentation
   * Licence
   * References
   * Open Source

Description
-----------

The HTML parser converts HTML to a format we call *lhtml*,
which is a natural encoding of HTML as Lisp lists.
*lhtml* can be converted back into HTML using the
htmlgen package that is part of
[AllegroServe](https://github.com/franzinc/aserve/).

This XML parser is a non-validating parser that converts XML into
*lxml*, a format similar to *lhtml*.  For a validating XML parser, see
the [SAX
module](http://www.franz.com/support/documentation/current/doc/sax.htm)
which is part of Allegro Common Lisp.

We consider the XML parser included here to be at end of life, and
recommend that you look at the SAX module mentioned above.

Authors
-------

Steve Jacobson and John Foderaro, 
with additional work by Steve Haflich, and others at Franz Inc.

Compatibility
-------------

The HTML parser works on Allegro Common Lisp versions 5.0.1 
and later although if your HTML code includes 16-bit character 
entities the parser will only be able to represent these 
characters in the International version of Allegro Common Lisp
version 6.0 or later.

Documentation
-------------

[HTML parser](https://github.com/franzinc/xmlutils/blob/master/phtml.htm)
and [XML parser](https://github.com/franzinc/xmlutils/blob/master/pxml.htm)
documentation are included in the repository.

License
-------

The source code is licensed under the terms of the
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the
LGPL. Where these conflict, the preamble takes precedence.  This project is
referenced in the preamble as the LIBRARY.

References
----------

For other links that may be of interest are:

 * [HTML 4.01 Specification](http://www.w3.org/TR/html4/)
 * [www.xml.org](http://www.xml.org)

Franz Inc. Open Source Info
---------------------------
      
This project's homepage is <http://opensource.franz.com>. There is an
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html)
for these open source projects. We encourage you to take advantage by
subscribing to the list.  Once you're subscribed, email to
<opensource@franz.com> with your questions, comments, suggestions, and
patches.
