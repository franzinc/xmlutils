(eval-when (compile load eval)
  (require :tester))

(defpackage :user (:use :net.uri :net.xml.parser))  ;; assumes pxml.cl loaded
(in-package :user)

(defvar *xml-test-string1*)
(defvar *xml-test-string2*)
(defvar *xml-test-string3*)
(defvar *xml-test-string4*)
(defvar *xml-test-string5*)
(defvar *xml-test-string6*)
(defvar *xml-test-string6a*)
(defvar *xml-test-string6b*)
(defvar *xml-test-string6c*)
(defvar *xml-test-string6d*)
(defvar *xml-test-string6e*)
(defvar *xml-test-string6f*)
(defvar *xml-test-string6g*)
(defvar *xml-test-string6h*)
(defvar *xml-test-string6i*)
(defvar *xml-test-string6j*)
(defvar *xml-test-string6k*)
(defvar *xml-test-string6l*)
(defvar *xml-test-string6m*)
(defvar *xml-test-string6n*)
(defvar *xml-test-string6o*)
(defvar *xml-test-string6p*)
(defvar *xml-test-string6q*)
(defvar *xml-test-string6r*)
(defvar *xml-test-string6s*)
(defvar *xml-test-string6t*)
(defvar *xml-test-string6u*)
(defvar *xml-test-string6v*)
(defvar *xml-test-string6w*)
(defvar *xml-test-string6x*)
(defvar *xml-test-string6y*)
(defvar *xml-test-string6z*)
(defvar *xml-test-string6aa*)
(defvar *xml-test-string6ab*)
(defvar *xml-test-string6ac*)
(defvar *xml-test-string6ad*)
(defvar *xml-test-string6ae*)
(defvar *xml-test-string6af*)
(defvar *xml-test-string6ag*)
(defvar *xml-test-string6ah*)
(defvar *xml-test-string6ai*)
(defvar *xml-test-string6aj*)
(defvar *xml-test-string6ak*)
(defvar *xml-test-string6al*)
(defvar *xml-test-string6am*)
(defvar *xml-test-string6an*)
(defvar *xml-test-string6ao*)
(defvar *xml-test-string6ap*)
(defvar *xml-test-string6aq*)
(defvar *xml-test-string6ar*)
(defvar *xml-test-string6as*)
(defvar *xml-test-string6at*)
(defvar *xml-test-string6au*)
(defvar *xml-test-string6av*)
(defvar *xml-test-string6aw*)
(defvar *xml-test-string6ay*)
(defvar *xml-test-string6az*)
(defvar *xml-test-string6ba*)
(defvar *xml-test-string6bb*)
(defvar *xml-test-string6bc*)
(defvar *xml-test-string6bd*)
(defvar *xml-test-string6be*)
(defvar *xml-test-string6bf*)
(defvar *xml-test-string6bg*)
(defvar *xml-test-string6bh*)
(defvar *xml-test-string6bi*)
(defvar *xml-test-string6bj*)
(defvar *xml-test-string6bk*)
(defvar *xml-test-string6bl*)
(defvar *xml-test-string6bm*)
(defvar *xml-test-string6bn*)
(defvar *xml-test-string6bo*)
(defvar *xml-test-string6bp*)
(defvar *xml-test-string6bq*)
(defvar *xml-test-string6br*)
(defvar *xml-test-string6bs*)
(defvar *xml-test-string6bt*)
(defvar *xml-test-string6bu*)
(defvar *xml-test-string6bv*)
(defvar *xml-test-string6bw*)
(defvar *xml-test-string6bx*)
(defvar *xml-test-string6by*)
(defvar *xml-test-string6bz*)
(defvar *xml-test-string6ca*)
(defvar *xml-test-string6cb*)
(defvar *xml-test-string6cc*)
(defvar *xml-test-string6cd*)
(defvar *xml-test-string6ce*)
(defvar *xml-test-string6cf*)
(defvar *xml-test-string6cg*)
(defvar *xml-test-string6ch*)
(defvar *xml-test-string6ci*)
(defvar *xml-test-string6cj*)
(defvar *xml-test-string6ck*)
(defvar *xml-test-string6cl*)
(defvar *xml-test-string6cm*)
(defvar *xml-test-string6cn*)
(defvar *xml-test-string6co*)
(defvar *xml-test-string6cp*)
(defvar *xml-test-string6cq*)
(defvar *xml-test-string6cr*)
(defvar *xml-test-string6cs*)
(defvar *xml-test-string6ct*)
(defvar *xml-test-string6cu*)
(defvar *xml-test-string6cv*)
(defvar *xml-test-string6cw*)
(defvar *xml-test-string6cx*)
(defvar *xml-test-string6cy*)
(defvar *xml-test-string6cz*)
(defvar *xml-test-string6da*)
(defvar *xml-test-string6db*)
(defvar *xml-test-string6dc*)
(defvar *xml-test-string6dd*)
(defvar *xml-test-string6de*)
(defvar *xml-test-string6df*)
(defvar *xml-test-string6dg*)
(defvar *xml-test-string6dh*)
(defvar *xml-test-string6di*)
(defvar *xml-test-string6dj*)
(defvar *xml-test-string6dk*)
(defvar *xml-test-string6dl*)
(defvar *xml-test-string6dm*)
(defvar *xml-test-string6dn*)
(defvar *xml-test-string6do*)
(defvar *xml-test-string6dp*)
(defvar *xml-test-string6dq*)
(defvar *xml-test-string6dr*)
(defvar *xml-test-string6ds*)
(defvar *xml-test-string6dt*)
(defvar *xml-test-string6du*)
(defvar *xml-test-string6dv*)
(defvar *xml-test-string6dw*)
(defvar *xml-test-string6dx*)
(defvar *xml-test-string6dy*)
(defvar *xml-test-string6dz*)
(defvar *xml-test-string6ea*)
(defvar *xml-test-string6eb*)
(defvar *xml-test-string6ec*)
(defvar *xml-test-string6ed*)
(defvar *xml-test-string6ee*)
(defvar *xml-test-string6ef*)
(defvar *xml-test-string6eg*)
(defvar *xml-test-string6eh*)
(defvar *xml-test-string6ei*)
(defvar *xml-test-string6ej*)
(defvar *xml-test-string6ek*)
(defvar *xml-test-string6el*)
(defvar *xml-test-string6em*)
(defvar *xml-test-string7*)
(defvar *xml-test-string8*)
(defvar *xml-test-string9*)
(defvar *xml-test-string10*)
(defvar *xml-test-string11*)
(defvar *xml-test-string12*)
(defvar *xml-test-string13*)
(defvar *xml-test-string14*)
(defvar *xml-test-string15*)
(defvar *xml-test-string16*)
(defvar *xml-test-string17*)
(defvar *xml-test-string18*)
(defvar *xml-test-string19*)
(defvar *xml-test-string20*)
(defvar *xml-test-string21*)
(defvar *xml-test-string22*)
(defvar *xml-test-string23*)
(defvar *xml-test-string24*)
(defvar *xml-test-string25*)
(defvar *xml-test-string26*)
(defvar *xml-test-string27*)
(defvar *xml-test-string28*)

(setf *xml-test-string1*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu  SYSTEM  \"this is a 'system' string\">
   <item/>
     ")

(setf *xml-test-string2*
  "<!-- this is a starting -comment- -->
   <!DOCTYPE menu >
   blah blah blah
   ")

(setf *xml-test-string3*
  "<?psprinter psprint -?- -g1000 paper=\"8.5x11\" ?>
   <?psprinter2    psprint -?- -g1000 paper=\"8.5x11\" ?>
   <!DOCTYPE menu  SYSTEM  'this is a system string'>
   <item/>
   ")

(setf *xml-test-string4*
  "
   <!-- this is a comment -->
   <!DOCTYPE menu[]>
   <item att1=\"'one'\" att2='this contains a char ref:&#x3c; &gt;,&#60; and a real &entity;'/>
   ")

(setf *xml-test-string5*
  "<!DOCTYPE menu>
   <item ></item >
   ")

;; intentional errors
(setf *xml-test-string6*
  "<!DOCTYPE>")

(setf *xml-test-string6a*
  "this is some bogus opening text")

(setf *xml-test-string6b*
  "<![CDATA[ these are chars that won't have entities ]]&gt ; ,]>&entity2; parsed]]>")

(setf *xml-test-string6c*
  "<!DOCTYPE menu  PUBLIC  \"this is a 'public' string\" 'this is the system string'>
   <![CDATA[ these are chars that won't have entities ]]&gt ; ,]>&entity2; parsed]]>")

(setf *xml-test-string6d*
  "<!DOCTYPE menu  PUBLIC  \"this is a 'public' string\" 'this is the system string'>
  this is some bogus content")

(setf *xml-test-string6e*
  "<!DOCTYPE menu>")

(setf *xml-test-string6f*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
  this is some bogus content")

(setf *xml-test-string6g*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <![CDATA[ these are chars that won't have entities ]]&gt ; ,]>&entity2; parsed]]>")

(setf *xml-test-string6h*
  "<!DOCTYPE menu>
   <item ></item2 >
   ")

(setf *xml-test-string6i*
  "<!DOCTYPE menu>
   <item >]]></item >
   ")

(setf *xml-test-string6j*
  "<!DOCTYPE menu>
   <item ></item >
   this is some bogus text
   ")

(setf *xml-test-string6k*
  "<!DOCTYPE menu>
   <item ></item >
   <![CDATA[ these are chars that won't have entities ]]&gt ; ,]>&entity2; parsed]]>
   ")

(setf *xml-test-string6l*
  "<item> this is a bad &&reference;? </item>")

(setf *xml-test-string6m*
  "<item> this is a bad &#a;? </item>")

(setf *xml-test-string6n*
  "<item> this is a reference without the semicolon: &ref</item>")

(setf *xml-test-string6o*
  "<item> this is a bad &#xk;? </item>")

(setf *xml-test-string6p*
  "<item> this is a bad &#9a;? </item>")

(setf *xml-test-string6q*
  "<item>  </5item>")

(setf *xml-test-string6r*
  "<item>  </item&>")

(setf *xml-test-string6s*
  "<item>  </item x>")

(setf *xml-test-string6t*
  "< item>  </item >")

(setf *xml-test-string6u*
  "<! DOCTYPE>")

(setf *xml-test-string6v*
  "<![DOCTYPE>")

(setf *xml-test-string6w*
  "<![CDATX[some chars]]>")

(setf *xml-test-string6x*
  "<!-CDATX[some chars]]>")

(setf *xml-test-string6y*
  "<!--DATX[some chars]]-- >")

(setf *xml-test-string6z*
  "<item& />")

(setf *xml-test-string6aa*
  "<item &>")

(setf *xml-test-string6ab*
  "<item attrib1/>")

(setf *xml-test-string6ac*
  "<item attrib1=hahaha/>")

(setf *xml-test-string6ad*
  "<item attrib1='hahaha/> <!-- comment -->")

(setf *xml-test-string6ae*
  "<item attrib1='ha&;haha'/>")

(setf *xml-test-string6af*
  "<item attrib1='ha&#a;haha'/>")

(setf *xml-test-string6ag*
  "<item attrib1='ha&#xag;haha'/>")

(setf *xml-test-string6ah*
  "<item attrib1='ha&#1a;haha'/>")

(setf *xml-test-string6ai*
  "<item attrib1='ha&a&;haha'/>")

(setf *xml-test-string6aj*
  "<item/ >")

(setf *xml-test-string6ak*
  "<?<")

(setf *xml-test-string6al*
  "<!DOCTYPE menu<")

(setf *xml-test-string6am*
  "<!DOCTYPE menu <")

(setf *xml-test-string6an*
  "<!DOCTYPE menu S<")

(setf *xml-test-string6ao*
  "<!DOCTYPE menu SXXX ")

(setf *xml-test-string6ap*
  "<!DOCTYPE menu PUBLIC xxx>")

(setf *xml-test-string6aq*
  "<!DOCTYPE menu SYSTEM xxx>")

(setf *xml-test-string6ar*
  "<!DOCTYPE menu PUBLIC \"this is bad <\"")

(setf *xml-test-string6as*
  "<!DOCTYPE menu PUBLIC 'this is bad <'")

(setf *xml-test-string6at*
  "<!DOCTYPE menu PUBLIC 'stuff' xxx>")

(setf *xml-test-string6au*
  "<!DOCTYPE menu PUBLIC 'stuff' [] xxx>")

(setf *xml-test-string6av*
  "<?xml <xx")

(setf *xml-test-string6aw*
  "<?xml xx<")

(setf *xml-test-string6ay*
  "<?xml xx=one")

(setf *xml-test-string6az*
  "<?xml xx='o<ne'")

(setf *xml-test-string6ba*
  "<?xml xx='one' ? >")

;; tests EOF in next-token
(setf *xml-test-string6bb*
  "<!DOCTYPE menu PUBLIC 'stuff' 'stuff' [")

(setf *xml-test-string6bc*
  "<!DOCTYPE menu PUBLIC 'stuff' 'stuff' [ ")

(setf *xml-test-string6bd*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [bad] ")

(setf *xml-test-string6be*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<bad] ")

(setf *xml-test-string6bf*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!<] ")

(setf *xml-test-string6bg*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!E<] ")

(setf *xml-test-string6bh*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT <] ")

(setf *xml-test-string6bi*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item<] ")

(setf *xml-test-string6bj*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item <] ")

(setf *xml-test-string6bk*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item BAD>] ")

(setf *xml-test-string6bl*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item BAD >] ")

(setf *xml-test-string6bm*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item BAD] ")

(setf *xml-test-string6bn*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item EMPTY ] ")

(setf *xml-test-string6bo*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (] ")

(setf *xml-test-string6bp*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA> ")

(setf *xml-test-string6bq*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATAX> ")

(setf *xml-test-string6br*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA) x> ")

(setf *xml-test-string6bs*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA > ")

(setf *xml-test-string6bt*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA) *> ")

(setf *xml-test-string6bu*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA)*!> ")

(setf *xml-test-string6bv*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA|)*> ")

(setf *xml-test-string6bw*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA|item*> ")

(setf *xml-test-string6bx*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA|item) *> ")

(setf *xml-test-string6by*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (item1>) *> ")

(setf *xml-test-string6bz*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (item1)&> ")

(setf *xml-test-string6ca*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item (#PCDATA| item8 *) >]>")

(setf *xml-test-string6cb*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item13 (item1?*)+ >")

(setf *xml-test-string6cc*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item14 (item1|)")

(setf *xml-test-string6cd*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item14 (item1|item2>)")

(setf *xml-test-string6ce*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item14 (item1|item2**")

(setf *xml-test-string6cf*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item14 (item1|item2))")

(setf *xml-test-string6cg*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item14 (item1|()")

(setf *xml-test-string6ch*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' [<!ELEMENT item20 ((item1+,item2) | ((item3))>")

(setf *xml-test-string6ci*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ELEMENT item20 ((item1+,item2) | ((item3))))>]><item />")

(setf *xml-test-string6cj*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST >")

(setf *xml-test-string6ck*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item*>")

(setf *xml-test-string6cl*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item *>")

(setf *xml-test-string6cm*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx*>")

(setf *xml-test-string6cn*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx bogus >")

(setf *xml-test-string6co*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx bogus* >")

(setf *xml-test-string6cp*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA bogus >")

(setf *xml-test-string6cq*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA #bogus >")

(setf *xml-test-string6cr*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA #FIXED>")

(setf *xml-test-string6cs*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA #FIXED*>")

(setf *xml-test-string6ct*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA 'bogus<'>")

(setf *xml-test-string6cu*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA '&*'>")

(setf *xml-test-string6cv*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA '&a*'>")

(setf *xml-test-string6cw*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA '&#*'>")

(setf *xml-test-string6cx*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA '&#6*'>")

(setf *xml-test-string6cy*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA '&#x6*'>")

(setf *xml-test-string6cz*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item att1 NOTATION >")

(setf *xml-test-string6da*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item att1 NOTATION ()>")

(setf *xml-test-string6db*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item att1 NOTATION (item*)>")

(setf *xml-test-string6dc*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item xx CDATA #FIXED *>")

(setf *xml-test-string6dd*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ATTLIST item att1 NOTATION (item *)>")

(setf *xml-test-string6de*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY >")

(setf *xml-test-string6df*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a>")

(setf *xml-test-string6dg*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a a>")

(setf *xml-test-string6dh*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a 'a>'")

(setf *xml-test-string6di*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a 'a<'")

(setf *xml-test-string6dj*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a 'a' *>")

(setf *xml-test-string6dk*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a a' *>")

(setf *xml-test-string6dl*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a abc *>")

(setf *xml-test-string6dm*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM *>")

(setf *xml-test-string6dn*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha'*>")

(setf *xml-test-string6do*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha' *>")

(setf *xml-test-string6dp*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha' NDATX >")

(setf *xml-test-string6dq*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha' NDATX* >")

(setf *xml-test-string6dr*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha' NDATA >")

(setf *xml-test-string6ds*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a SYSTEM 'ha' NDATA a*>")

(setf *xml-test-string6dt*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY a PUBLIC *")

(setf *xml-test-string6du*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITYX a PUBLIC *")

(setf *xml-test-string6dv*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!NOTATION *a PUBLIC *")

(setf *xml-test-string6dw*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!NOTATION a*a PUBLIC *")

(setf *xml-test-string6dx*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!NOTATION aa *PUBLIC *")

(setf *xml-test-string6dy*
  "<? this should be an error ?>
   <item/>")

(setf *xml-test-string6dz*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY %a PUBLIC *")

(setf *xml-test-string6ea*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY % entity1 SYSTEM  \"this is a 'system' string\" NDATA somedata>]>
   <item/>")

(setf *xml-test-string6eb*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [%*xxx")

(setf *xml-test-string6ec*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [%xxx*")

(setf *xml-test-string6ed*
  "<!DOCTYPE menu PUBLIC 'stuff'  'stuff' 
     [<!ENTITY % *entity1 SYSTEM  \"this is a 'system' string\" NDATA somedata>]>
   <item/>")

(setf *xml-test-string6ee*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu[]>
   <?xml version='1.0' encoding=\"UTF-8\" ?>")

(setf *xml-test-string6ef*
  "<?xml >
   <!DOCTYPE menu[]>
   <item/>")

(setf *xml-test-string6eg*
  "<?xml xxx='1' ?>
   <!DOCTYPE menu[]>
   <!DOCTYPE menu[]>
   <item/>")

(setf *xml-test-string6eh*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
     'this is some content'
     <!ENTITY % pentity1 'this is string 1'>")

(setf *xml-test-string6ei*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
     <!ENTITY % pentity1 'this is string 1'>
     'this is some content'
     ")

(setf *xml-test-string6ej*
  "<![IXCLUDE[<!ENTITY % pentity1 'this is string 1'>]]>
   <![ IGNORE [ <!ENTITY % pentity2 'this is string 2'>]]>
   %pentity1;
   %pentity2;")

(setf *xml-test-string6ek*
  "<!ENTITY % pentity1 'this is string 1'>]")

;; call without :external
(setf *xml-test-string6el*
  "<![INCLUDE <!ENTITY % pentity1 'this is string 1'>]>")

(setf *xml-test-string6em*
  "<![INXCLUDE[<!ENTITY % pentity1 'this is string 1'>]]>
   <![ IGNORE [ <!ENTITY % pentity2 'this is string 2'>]]>
   %pentity1;
   %pentity2;")

(setf *xml-test-string7*
  "<!DOCTYPE menu  PUBLIC  \"this is a 'public' string\" 'this is the system string'>
   <item><item2><item4>this is some content with OK ]]- ]-> chars</item4>
         this is content with some refs: &#x3c; &#60; &amp; &entity2;, followed by more content
         </item2>
     <![CDATA[ these are chars that won't have entities ]]&gt; ,]>&entity2; parsed]]>
     <?psprinter psprint -?- -g1000 paper=\"8.5x11\" ?>
      <!-- this is a comment -->
     <item3/></item>
     
     <!-- this is another comment -->
     <?psprinter2    psprint -?- -g1000 paper=\"8.5x11\" ?>
     ")

(setf *xml-test-string8*
  "<!DOCTYPE menu  PUBLIC  'this is a public string' 'this is the system string' >
   <!-- this is a -comment- -->
   <item att1='one' att2='two'/>
     ")

(setf *xml-test-string9*
  "<item/>
   ")

(setf *xml-test-string10*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu[
    <!ELEMENT item EMPTY >
   ]>
   <item />
   ")

(setf *xml-test-string11*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu[
    <!ENTITY % pentity1 'this is a string'>
    <!ENTITY % entity1f PUBLIC  \"this is a 'public' string\" 'system'>
    <!ENTITY % entity1g PUBLIC  \"this is a 'public' string\"  'system' >
    <!ENTITY % entity1h \"<!ENTITY entity1ii 'this is a string'>\">
    %entity1h;
    <!-- this is a starting -comment- -->
    <?psprinter psprint -?- -g1000 paper=\"8.5x11\" ?>
    <!NOTATION item1 SYSTEM  \"this is a 'system' string\">
    <!NOTATION item2 SYSTEM  \"this is a 'system' string\" >
    <!NOTATION item3 PUBLIC  \"this is a 'public' string\" 'this is the system string'>
    <!NOTATION item4 PUBLIC  \"this is a 'public' string\">
    <!NOTATION item5 PUBLIC  \"this is a 'public' string\" >
    <!ELEMENT item ANY>
    <!ELEMENT item1 (#PCDATA) >
    <!ELEMENT item2 (#PCDATA)>
    <!ELEMENT item3 (#PCDATA )>
    <!ELEMENT item4 (#PCDATA )*>
    <!ELEMENT item5 (#PCDATA|item4)*>
    <!ELEMENT item6 (#PCDATA |item4)*>
    <!ELEMENT item7 (#PCDATA| item4)*>
    <!ELEMENT item8 (#PCDATA| item4 )*>
    <!ELEMENT item9 (#PCDATA | item4 | item5)*>
    <!ELEMENT item10 (item1)>
    <!ELEMENT item11 (item1)*>
    <!ELEMENT item12 (item1*)?>
    <!ELEMENT item13 (item1?)+ >
    <!ELEMENT item14 (item1|item2+)+ >
    <!ELEMENT item15 (item1 |item2)>
    <!ELEMENT item16 (item1 | item2 | item4)>
    <!ELEMENT item17 (item1+ | (item3,item4))+ >
    <!ELEMENT item18 ((item1+,item2) | (item3,item4))+ >
    <!ELEMENT item19 ((item1+,item2) | ((item3,item4) | item5))+ >
    <!ELEMENT item20 ((item1+,item2) | ((item3)))>
    <!ELEMENT   item21  (item1)  >
    <!ATTLIST item1>
    <!ATTLIST item2 >
    <!ATTLIST item3 att1 CDATA #REQUIRED>
    <!ATTLIST item4 att1 ID #IMPLIED >
    <!ENTITY entity1 \"haha\">
    <!ENTITY entity1a  'haha2' >
    <!ENTITY entity1b 'haha3'  >
    <!ENTITY entity1c '%pentity1; &entity1; &gt; &#60; &#x3c;'>
    <!ENTITY entity1d SYSTEM  \"this is a 'system' string\">
    <!ENTITY entity1e SYSTEM  \"this is a 'system' string\" NDATA somedata>
    <!ENTITY entity1f PUBLIC  \"this is a 'public' string\" 'this is the system string'>
    <!ENTITY entity1g PUBLIC  \"this is a 'public' string\" 'this is the system string'
                              NDATA moredata>
    <!ENTITY entity1h SYSTEM  \"this is a 'system' string\" >
    <!ENTITY entity1i PUBLIC  \"this is a 'public' string\" 'this is the system string' >
    <!ATTLIST item5 att1 IDREF \"val1\"
                    att2 IDREFS #FIXED 'val2'>
    <!ATTLIST item6 att1 ENTITY '&gt; &#60; &#x3c; &entity2;zzz'>
    <!ATTLIST item7 att1 ENTITIES #IMPLIED
                    att2 NMTOKEN #IMPLIED
                    att3 NMTOKENS #IMPLIED
                    att3a  NMTOKENS  #IMPLIED
                    att4 NOTATION (name1) #REQUIRED
                    att5 NOTATION (name1 ) #REQUIRED
                    att6 NOTATION (name1|name2) #REQUIRED
                    att7 NOTATION ( name1| name2) #REQUIRED
                    att8 NOTATION ( name1 |name2) #REQUIRED
                    att9 (name1) #REQUIRED
                    att10 (name1 ) #REQUIRED
                    att11 (name1|name2) #REQUIRED
                    att12 ( name1| name2) #REQUIRED
                    att13 ( name1 |name2) #REQUIRED>
   ]>
   <item />"
  )

(setf *xml-test-string12*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu[
    <!ENTITY % pentity1 'this is string 1'>
    <!ENTITY % pentity2 \"<!ENTITY entity1 'this is string 2'>\">
    %pentity2;
    %pentity3;
    <!ENTITY entity2 '%pentity1; plus some more'>
    <!ENTITY entity3 '&entity2; -- plus even more'>
    ]>
   <item att1='&entity1;'>
     this is some text that includes &entity2; and &entity3; entities.
   </item>")

(setf *xml-test-string13*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
     <!ENTITY % pentity1 'this is string 1'>")

(setf *xml-test-string14*
  "<!ENTITY % pentity1 'this is string 1'>")

(setf *xml-test-string15*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
     'this is some content'")

(setf *xml-test-string16*
  "<item>contents</item>")

(setf *xml-test-string17*
  "<item>contents</item><item2>more contents</item2>")

(setf *xml-test-string18*
  "<![INCLUDE[<!ENTITY % pentity1 \"<!ENTITY entity3 'this is string 1'>\">]]>
   <![ IGNORE [ <!ENTITY % pentity2 'this is string 2'>]]>
   %pentity1;
   %pentity2;")

(setf *xml-test-string19*
  "<![   IGNORE[<![INCLUDE[<!ENTITY % pentity1 \"<!ENTITY entity3 'this is string 1'>\">]]>
   <![ IGNORE [ <!ENTITY % pentity2 'this is string 2'>]]>]]>
   %pentity1;
   %pentity2;")

(defvar *entity1* "<!ENTITY entity1 'here is some external text'>")

(defun file-callback (filename token &optional public)
  (declare (ignorable token public))
  ;;(format t "filename: ~s token: ~s public: ~s~%" filename token public)
  (ignore-errors (open (uri-path filename))))

(defun external-callback (arg)
  (if* (eq (first arg) :DOCTYPE) then
	  (if* (eq (third arg) :SYSTEM) then
		  (let ((arg2 (eval (intern (fourth arg) (find-package :user)))))
		    (if* (= (length arg2) 0) then t
		       else (parse-xml arg2 :external t)))
	     else (let ((arg2 (eval (intern (fifth arg) (find-package :user)))))
		    (if* (= (length arg2) 0) then t
		       else (parse-xml arg2 :external t))))
   elseif (eq (first arg) :ENTITY) then
	  (let ((arg2 (eval (intern (first (last arg)) (find-package :user)))))
	    (if* (= (length arg2) 0) then t
	       else (parse-xml arg2 :external t)))
     else	  
	  (break "in callback; arg: ~s" arg)))

(setf *xml-test-string20*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu  SYSTEM  \"*entity1*\">
   <item>this is an entity string-&entity1;-</item>
     ")

(setf *xml-test-string21*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu  PUBLIC 'public string'  \"*entity1*\">
   <item>this is an entity string-&entity1;-</item>
     ")
(defvar *entity2* "this is a line of text")

(defvar *entity3* "<item2> here is some content<item3> more content</item3> and more </item2>")

(setf *xml-test-string22*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu [
      <!ENTITY % pentity1 SYSTEM \"*entity1*\">
      <!ENTITY % pentity2 SYSTEM \"*entity2*\">
      <!ENTITY entity2 'here is: %pentity2; and some more'>
      <!ENTITY entity3 SYSTEM '*entity3*'>
      <!ATTLIST item att1 ENTITY 'some test &entity2; and some more'>
      %pentity1; ]>
   <item att1='test:&entity2;:more'>this is an entity string-&entity1;-&entity2;-&entity3;</item>
     ")

;; should generate error
(setf *xml-test-string23*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu [
      <!ENTITY % pentity1 SYSTEM \"*entity2*\">
      %pentity1; ]>
   <item>this is an entity string-&entity1;-</item>
     ")

;; should generate error
(setf *xml-test-string24*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu [
      <!ENTITY % pentity1 SYSTEM \"*entity1*\">
      <!ENTITY % pentity2 SYSTEM \"*entity1*\">
      <!ENTITY entity2 'here is: %pentity2; and some more'>
      %pentity1; ]>
   <item>this is an entity string-&entity1;-&entity2;-</item>
     ")

;; should generate error
(setf *xml-test-string25*
  "<?xml version='1.0' encoding=\"UTF-8\" ?>
   <!DOCTYPE menu [
      <!ENTITY % pentity1 SYSTEM \"*entity1*\">
      <!ENTITY % pentity2 SYSTEM \"*entity1*\">
      <!ENTITY entity2 'here is: %pentity2; and some more'>
      <!ATTLIST item att1 ENTITY 'some test &entity2; and some more'>
      %pentity1; ]>
   <item>this is an entity string-&entity1;-&entity2;-</item>
     ")

(defvar *entity4* "")

;; test to see that unparsed external entity turns off entity substitution
(setf *xml-test-string26*
  "<!DOCTYPE menu SYSTEM '*entity4*' [
     <!ENTITY entity2 'here is some text'> ]>
   <item> here is the &entity2; inserted and some &entity3;</item>")

;; this should fail
(setf *xml-test-string27*
  "<item> this is some text with bad contents: ]]> </item>")

;; this should work
(setf *xml-test-string28*
  "<item> this is some text with foo contents: ]]&gt; </item>")

(defun test-one-file (int external-callback)
  (let ((filename (concatenate 'string (format nil "~3,'0d" int) ".xml")))
    (equalp (with-open-file (p filename) 
	      (parse-xml p :external-callback external-callback
			 :content-only t))
	    (with-open-file (p (concatenate 'string "out/" filename))
	      (parse-xml p)))))

(defun test-some-files (max &key skip-list external-callback)
  (dotimes (i max)
    (if* (member (+ 1 i) skip-list) then
	    (format t "i: ~s skipping...~%" (+ 1 i))
       else
	    (format t "i: ~s equalp: ~s~%" (+ 1 i) (test-one-file (+ 1 i) external-callback)))))

;; have to be in sa directory
(defun test-sa-files ()
  (test-some-files 119 :external-callback 'file-callback :skip-list (list 52 64 89)))

(defun test-ext-sa-files ()
  (test-some-files 14 :external-callback 'file-callback ))

(defun test-not-sa-files ()
  (test-some-files 31 :external-callback 'file-callback ))

(defun test-one-bad-file (filename external-callback)
  (ignore-errors
   (with-open-file (p filename) 
     (parse-xml p :external-callback external-callback
		:content-only t))))

(defun test-some-bad-files (max external-callback)
  (dotimes (i max)
    (let* ((index (+ 1 i))
	   (filename (concatenate 'string (format nil "~3,'0d" index) ".xml")))
      (multiple-value-bind (val error)
	  (test-one-bad-file filename external-callback) 
	(format t "i: ~s error: ~s~%"
		index (if error
			  (simple-condition-format-control error) val))))))

(defun test-not-wf-sa-files ()
  (test-some-bad-files 186 'file-callback))

(defun test-not-wf-ext-sa-files ()
  (test-some-bad-files 3 'file-callback))

(defun test-not-wf-not-sa-files ()
  (test-some-bad-files 8 'file-callback))

;; the next stuff is used in the .txt file for documentation

(defvar *xml-example-external-url*
    "<!ENTITY ext1 'this is some external entity %param1;'>")

(defun example-callback (var-name token &optional public)
  (declare (ignorable token public))
  (setf var-name (uri-path var-name))
  (if* (equal var-name "null") then nil
     else
	  (let ((string (eval (intern var-name (find-package :user)))))
	    (make-string-input-stream string))))

(defvar *xml-example-string*
    "<?xml version='1.0' encoding='utf-8'?>
<!-- the following XML input is well-formed but its validity has not been checked ... -->
<?piexample this is an example processing instruction tag ?>
<!DOCTYPE example SYSTEM '*xml-example-external-url*' [
   <!ELEMENT item1 (item2* | (item3+ , item4))>
   <!ELEMENT item2 ANY>
   <!ELEMENT item3 (#PCDATA)>
   <!ELEMENT item4 (#PCDATA)>
   <!ATTLIST item1
        att1 CDATA #FIXED 'att1-default'
        att2 ID #REQUIRED
        att3 ( one | two | three ) 'one'
        att4 NOTATION ( four | five ) 'four' >
   <!ENTITY % param1 'text'>
   <!ENTITY nentity SYSTEM 'null' NDATA somedata>
   <!NOTATION notation SYSTEM 'notation-processor'>
]>
<item1 att2='1'><item3>&ext1;</item3></item1>")

(defvar *xml-example-string2*)
(defvar *xml-example-string3*)

;; bug fix testing
(setf *xml-example-string2*
    "<!DOCTYPE example [
<!ELEMENT item1 (item2* | (item3+ , item4))>
]>
<item1/>")

(setf *xml-example-string3*
    "<!DOCTYPE example [
<!ELEMENT item1 (item2* | (item3+ , item4*))>
]>
<item1/>")


