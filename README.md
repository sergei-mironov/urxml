UrXML
=====

UrXML is another XML parser/pretty-printer based on
(http://hackage.haskell.org/package/really-simple-xml-parser)[RSXP] package by
CK Kashyap.

UrXML is vim-friendly and can be used to pretty-print the XML-fragment of
non-XML file.

Additionaly, the tool is able to translate the usual XML into Ur/Web dialect of
XML (uses several hardcoded rules)

Usage
-----

    $ urxml --help
    XML converter/indenter

    Usage: urxml [-w|--text-width INT] [-m|--right-margin INT] [-i|--start-indent INT] [-s|--tab-stop INT] [-e|--expand-tab] [-1|--skip-first] [-S|--skip-schema] [-u|--ur-attrs] [FILE]
      Ur/Web XML indenter (Ur/Web dialect of XML supports {}-style attributes).

    Available options:
      -h,--help                Show this help text
      -w,--text-width INT      Recommended text width (not strict)
      -m,--right-margin INT    Right margin
      -i,--start-indent INT    Indent to start from
      -s,--tab-stop INT        Tab stop
      -e,--expand-tab          Expand tab
      -1,--skip-first          Don't indent 1st line
      -S,--skip-schema         Don't print DOCTYPE and stuff
      -u,--ur-attrs            Convert tag attributes to the Ur/Web format
      FILE                     File or `stdin' to read from the stdin

Examples
--------

Simple pretty-printing:

    $ urxml test.xml 
    <xml>
        <a>Under a</a>
        <b>
            <c a='b'>
                under c1 asd asd asd asd asd as dasd asd as dasdasd asd asdas dasd asdas das das
                dasdas dasdasdas das dasdasdasd
            </c>
            <c c='d'>under c2</c>
            <c number=33 width="33" height="44">under c3</c>
            <c src="images/asdasdad.gif" urcode={main
          {}}>under c4</c>
        </b>
    </xml>

Pretty-print with identation:

    $ urxml -i 5 test.xml 
         <xml>
             <a>Under a</a>
             <b>
                 <c a='b'>
                     under c1 asd asd asd asd asd as dasd asd as dasdasd asd asdas dasd asdas das das
                     dasdas dasdasdas das dasdasdasd
                 </c>
                 <c c='d'>under c2</c>
                 <c number=33 width="33" height="44">under c3</c>
                 <c src="images/asdasdad.gif" urcode={main {}}>under c4</c>
             </b>
         </xml>

Pretty-print with identation, but don't indent first line:

    $ urxml -i 5 -1 test.xml 
    <xml>
             <a>Under a</a>
             <b>
                 <c a='b'>
                     under c1 asd asd asd asd asd as dasd asd as dasdasd asd asdas dasd asdas das das
                     dasdas dasdasdas das dasdasdasd
                 </c>
                 <c c='d'>under c2</c>
                 <c number=33 width="33" height="44">under c3</c>
                 <c src="images/asdasdad.gif" urcode={main {}}>under c4</c>
             </b>
         </xml>

and so on.

VIM integration
---------------

UrXML is the easy way of formatting XML inside non-XML documents. Just paste
the following lines into your ~/.vimrc and format the XML by visually selecting
the fragment (note, that it should be a correct part of XML with matching tags)
and pressing the '!' key, as set in the last line of the fragment.

    " ~/.vimrc fragment
    function! ProgramFilter(vt, ...)
      normal `<
      let p = getpos ('.')

      let [qr, qt] = [getreg('"'), getregtype('"')]
      let [oai, ocin, osi, oinde] = [&ai, &cin, &si, &inde]
      setl noai nocin nosi inde=

      let [sm, em] = ['[<'[a:0], ']>'[a:0]]
      exe 'norm!`' . sm . a:vt . '`' . em . 'x'

      let tw = &tw
      setl tw=0

      let cmd = 'urxml -i ' . (p[2]-1) . ' -1 --tab-stop 2 --ur-attrs stdin'

      let out = system(cmd, @")
      let out = substitute(out, '\n\n$', '', '')
      exe "norm!i\<c-r>=out\r"

      let &tw = tw
      let [&ai, &cin, &si, &inde] = [oai, ocin, osi, oinde]
      call setreg('"', qr, qt)
    endfunction
    vnoremap <silent> ! :<c-u>call ProgramFilter(visualmode(), 1)<cr>

RSXP's README
-------------

The original project is located at

https://github.com/ckkashyap/really-simple-xml-parser

A really simple xml parser in Haskell using Parsec.
I wrote this primarily to learn Parsec and I intend to use it in my automations where I need to parse XML's returned from bug tracking systems and other systems.

I am not an XML fan - in fact, I hate it.

I'd like to acknowledge the help I got from the wonderful Haskell community for this. I'd particularly like to call out the help provided by the following folks -

1. Antoine Latter <aslatter@gmail.com>
1. Simon Hengel <sol@typeful.net>

