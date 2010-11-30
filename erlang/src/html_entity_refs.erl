-module(html_entity_refs).
-vsn('0.1').
-author('dcaoyuan@gmail.com').


-export([decode_for_xml/1]).

-export([get_xmerl_rules/0]).

-export([decode_for_xml_test_/0,
         xmerl_rules_test_/0]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec EncodedHtml::string() -> string()
%% @doc decode html entity references to utf-8 chars, except xml special entity refs:
%%      "&quot;" "&amp;" "&lt;" "&gt;"
%%
%%
decode_for_xml(EncodedHtml) -> decode_for_xml(EncodedHtml, []).
decode_for_xml([],              Decoded) -> lists:reverse(Decoded);
decode_for_xml("&nbsp;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#160, Decoded));  %% no-break space = non-breaking space, U+00A0 ISOnum -->
decode_for_xml("&iexcl;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#161, Decoded));  %% inverted exclamation mark, U+00A1 ISOnum -->
decode_for_xml("&cent;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#162, Decoded));  %% cent sign, U+00A2 ISOnum -->
decode_for_xml("&pound;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#163, Decoded));  %% pound sign, U+00A3 ISOnum -->
decode_for_xml("&curren;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#164, Decoded));  %% currency sign, U+00A4 ISOnum -->
decode_for_xml("&yen;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#165, Decoded));  %% yen sign = yuan sign, U+00A5 ISOnum -->
decode_for_xml("&brvbar;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#166, Decoded));  %% broken bar = broken vertical bar,
decode_for_xml("&sect;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#167, Decoded));  %% section sign, U+00A7 ISOnum -->
decode_for_xml("&uml;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#168, Decoded));  %% diaeresis = spacing diaeresis,
decode_for_xml("&copy;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#169, Decoded));  %% copyright sign, U+00A9 ISOnum -->
decode_for_xml("&ordf;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#170, Decoded));  %% feminine ordinal indicator, U+00AA ISOnum -->
decode_for_xml("&laquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#171, Decoded));  %% left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
decode_for_xml("&not;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#172, Decoded));  %% not sign, U+00AC ISOnum -->
decode_for_xml("&shy;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#173, Decoded));  %% soft hyphen = discretionary hyphen, U+00AD ISOnum -->
decode_for_xml("&reg;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#174, Decoded));  %% registered sign = registered trade mark sign, U+00AE ISOnum -->
decode_for_xml("&macr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#175, Decoded));  %% macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->
decode_for_xml("&deg;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#176, Decoded));  %% degree sign, U+00B0 ISOnum -->
decode_for_xml("&plusmn;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#177, Decoded));  %% plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->
decode_for_xml("&sup2;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#178, Decoded));  %% superscript two = superscript digit two = squared, U+00B2 ISOnum -->
decode_for_xml("&sup3;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#179, Decoded));  %% superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
decode_for_xml("&acute;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#180, Decoded));  %% acute accent = spacing acute, U+00B4 ISOdia -->
decode_for_xml("&micro;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#181, Decoded));  %% micro sign, U+00B5 ISOnum -->
decode_for_xml("&para;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#182, Decoded));  %% pilcrow sign = paragraph sign, U+00B6 ISOnum -->
decode_for_xml("&middot;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#183, Decoded));  %% middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
decode_for_xml("&cedil;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#184, Decoded));  %% cedilla = spacing cedilla, U+00B8 ISOdia -->
decode_for_xml("&sup1;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#185, Decoded));  %% superscript one = superscript digit one, U+00B9 ISOnum -->
decode_for_xml("&ordm;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#186, Decoded));  %% masculine ordinal indicator, U+00BA ISOnum -->
decode_for_xml("&raquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#187, Decoded));  %% right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
decode_for_xml("&frac14;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#188, Decoded));  %% vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
decode_for_xml("&frac12;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#189, Decoded));  %% vulgar fraction one half = fraction one half, U+00BD ISOnum -->
decode_for_xml("&frac34;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#190, Decoded));  %% vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
decode_for_xml("&iquest;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#191, Decoded));  %% inverted question mark = turned question mark, U+00BF ISOnum -->
decode_for_xml("&Agrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#192, Decoded));  %% latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
decode_for_xml("&Aacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#193, Decoded));  %% latin capital letter A with acute, U+00C1 ISOlat1 -->
decode_for_xml("&Acirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#194, Decoded));  %% latin capital letter A with circumflex, U+00C2 ISOlat1 -->
decode_for_xml("&Atilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#195, Decoded));  %% latin capital letter A with tilde, U+00C3 ISOlat1 -->
decode_for_xml("&Auml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#196, Decoded));  %% latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
decode_for_xml("&Aring;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#197, Decoded));  %% latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
decode_for_xml("&AElig;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#198, Decoded));  %% latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
decode_for_xml("&Ccedil;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#199, Decoded));  %% latin capital letter C with cedilla, U+00C7 ISOlat1 -->
decode_for_xml("&Egrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#200, Decoded));  %% latin capital letter E with grave, U+00C8 ISOlat1 -->
decode_for_xml("&Eacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#201, Decoded));  %% latin capital letter E with acute, U+00C9 ISOlat1 -->
decode_for_xml("&Ecirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#202, Decoded));  %% latin capital letter E with circumflex, U+00CA ISOlat1 -->
decode_for_xml("&Euml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#203, Decoded));  %% latin capital letter E with diaeresis, U+00CB ISOlat1 -->
decode_for_xml("&Igrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#204, Decoded));  %% latin capital letter I with grave, U+00CC ISOlat1 -->
decode_for_xml("&Iacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#205, Decoded));  %% latin capital letter I with acute, U+00CD ISOlat1 -->
decode_for_xml("&Icirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#206, Decoded));  %% latin capital letter I with circumflex, U+00CE ISOlat1 -->
decode_for_xml("&Iuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#207, Decoded));  %% latin capital letter I with diaeresis, U+00CF ISOlat1 -->
decode_for_xml("&ETH;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#208, Decoded));  %% latin capital letter ETH, U+00D0 ISOlat1 -->
decode_for_xml("&Ntilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#209, Decoded));  %% latin capital letter N with tilde, U+00D1 ISOlat1 -->
decode_for_xml("&Ograve;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#210, Decoded));  %% latin capital letter O with grave, U+00D2 ISOlat1 -->
decode_for_xml("&Oacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#211, Decoded));  %% latin capital letter O with acute, U+00D3 ISOlat1 -->
decode_for_xml("&Ocirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#212, Decoded));  %% latin capital letter O with circumflex, U+00D4 ISOlat1 -->
decode_for_xml("&Otilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#213, Decoded));  %% latin capital letter O with tilde, U+00D5 ISOlat1 -->
decode_for_xml("&Ouml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#214, Decoded));  %% latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
decode_for_xml("&times;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#215, Decoded));  %% multiplication sign, U+00D7 ISOnum -->
decode_for_xml("&Oslash;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#216, Decoded));  %% latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
decode_for_xml("&Ugrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#217, Decoded));  %% latin capital letter U with grave, U+00D9 ISOlat1 -->
decode_for_xml("&Uacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#218, Decoded));  %% latin capital letter U with acute, U+00DA ISOlat1 -->
decode_for_xml("&Ucirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#219, Decoded));  %% latin capital letter U with circumflex, U+00DB ISOlat1 -->
decode_for_xml("&Uuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#220, Decoded));  %% latin capital letter U with diaeresis, U+00DC ISOlat1 -->
decode_for_xml("&Yacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#221, Decoded));  %% latin capital letter Y with acute, U+00DD ISOlat1 -->
decode_for_xml("&THORN;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#222, Decoded));  %% latin capital letter THORN, U+00DE ISOlat1 -->
decode_for_xml("&szlig;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#223, Decoded));  %% latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
decode_for_xml("&agrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#224, Decoded));  %% latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
decode_for_xml("&aacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#225, Decoded));  %% latin small letter a with acute, U+00E1 ISOlat1 -->
decode_for_xml("&acirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#226, Decoded));  %% latin small letter a with circumflex, U+00E2 ISOlat1 -->
decode_for_xml("&atilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#227, Decoded));  %% latin small letter a with tilde, U+00E3 ISOlat1 -->
decode_for_xml("&auml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#228, Decoded));  %% latin small letter a with diaeresis, U+00E4 ISOlat1 -->
decode_for_xml("&aring;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#229, Decoded));  %% latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
decode_for_xml("&aelig;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#230, Decoded));  %% latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
decode_for_xml("&ccedil;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#231, Decoded));  %% latin small letter c with cedilla, U+00E7 ISOlat1 -->
decode_for_xml("&egrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#232, Decoded));  %% latin small letter e with grave, U+00E8 ISOlat1 -->
decode_for_xml("&eacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#233, Decoded));  %% latin small letter e with acute, U+00E9 ISOlat1 -->
decode_for_xml("&ecirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#234, Decoded));  %% latin small letter e with circumflex, U+00EA ISOlat1 -->
decode_for_xml("&euml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#235, Decoded));  %% latin small letter e with diaeresis, U+00EB ISOlat1 -->
decode_for_xml("&igrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#236, Decoded));  %% latin small letter i with grave, U+00EC ISOlat1 -->
decode_for_xml("&iacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#237, Decoded));  %% latin small letter i with acute, U+00ED ISOlat1 -->
decode_for_xml("&icirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#238, Decoded));  %% latin small letter i with circumflex, U+00EE ISOlat1 -->
decode_for_xml("&iuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#239, Decoded));  %% latin small letter i with diaeresis, U+00EF ISOlat1 -->
decode_for_xml("&eth;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#240, Decoded));  %% latin small letter eth, U+00F0 ISOlat1 -->
decode_for_xml("&ntilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#241, Decoded));  %% latin small letter n with tilde, U+00F1 ISOlat1 -->
decode_for_xml("&ograve;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#242, Decoded));  %% latin small letter o with grave, U+00F2 ISOlat1 -->
decode_for_xml("&oacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#243, Decoded));  %% latin small letter o with acute, U+00F3 ISOlat1 -->
decode_for_xml("&ocirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#244, Decoded));  %% latin small letter o with circumflex, U+00F4 ISOlat1 -->
decode_for_xml("&otilde;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#245, Decoded));  %% latin small letter o with tilde, U+00F5 ISOlat1 -->
decode_for_xml("&ouml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#246, Decoded));  %% latin small letter o with diaeresis, U+00F6 ISOlat1 -->
decode_for_xml("&divide;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#247, Decoded));  %% division sign, U+00F7 ISOnum
decode_for_xml("&oslash;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#248, Decoded));  %% latin small letter o with stroke = latin small letter o slash, U+00F8 ISOlat1 -->
decode_for_xml("&ugrave;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#249, Decoded));  %% latin small letter u with grave, U+00F9 ISOlat1 -->
decode_for_xml("&uacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#250, Decoded));  %% latin small letter u with acute, U+00FA ISOlat1 -->
decode_for_xml("&ucirc;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#251, Decoded));  %% latin small letter u with circumflex, U+00FB ISOlat1 -->
decode_for_xml("&uuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#252, Decoded));  %% latin small letter u with diaeresis, U+00FC ISOlat1 -->
decode_for_xml("&yacute;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#253, Decoded));  %% latin small letter y with acute, U+00FD ISOlat1 -->
decode_for_xml("&thorn;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#254, Decoded));  %% latin small letter thorn, U+00FE ISOlat1 -->
decode_for_xml("&yuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#255, Decoded));  %% latin small letter y with diaeresis, U+00FF ISOlat1 -->


%% Special characters for HTML -->
%% C0 Controls and Basic Latin -->
%% @notice We should keep these char as it, since xml should encode them as it
%decode_for_xml("&quot;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#34, Decoded));  %% quotation mark = APL quote, U+0022 ISOnum -->
%decode_for_xml("&amp;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#38, Decoded));  %% ampersand, U+0026 ISOnum -->
%decode_for_xml("&lt;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#60, Decoded));  %% less-than sign, U+003C ISOnum -->
%decode_for_xml("&gt;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#62, Decoded));  %% greater-than sign, U+003E ISOnum -->

%% Latin Extended-A -->
decode_for_xml("&OElig;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#338, Decoded));  %% latin capital ligature OE, U+0152 ISOlat2 -->
decode_for_xml("&oelig;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#339, Decoded));  %% latin small ligature oe, U+0153 ISOlat2 -->
%% ligature is a misnomer, this is a separate character in some languages -->
decode_for_xml("&Scaron;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#352, Decoded));  %% latin capital letter S with caron, U+0160 ISOlat2 -->
decode_for_xml("&scaron;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#353, Decoded));  %% latin small letter s with caron, U+0161 ISOlat2 -->
decode_for_xml("&Yuml;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#376, Decoded));  %% latin capital letter Y with diaeresis, U+0178 ISOlat2 -->

%% Spacing Modifier Letters -->
decode_for_xml("&circ;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#710, Decoded));  %% modifier letter circumflex accent, U+02C6 ISOpub -->
decode_for_xml("&tilde;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#732, Decoded));  %% small tilde, U+02DC ISOdia -->

%% General Punctuation -->
decode_for_xml("&ensp;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8194, Decoded));  %% en space, U+2002 ISOpub -->
decode_for_xml("&emsp;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8195, Decoded));  %% em space, U+2003 ISOpub -->
decode_for_xml("&thinsp;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8201, Decoded));  %% thin space, U+2009 ISOpub -->
decode_for_xml("&zwnj;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8204, Decoded));  %% zero width non-joiner, U+200C NEW RFC 2070 -->
decode_for_xml("&zwj;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8205, Decoded));  %% zero width joiner, U+200D NEW RFC 2070 -->
decode_for_xml("&lrm;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8206, Decoded));  %% left-to-right mark, U+200E NEW RFC 2070 -->
decode_for_xml("&rlm;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8207, Decoded));  %% right-to-left mark, U+200F NEW RFC 2070 -->
decode_for_xml("&ndash;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8211, Decoded));  %% en dash, U+2013 ISOpub -->
decode_for_xml("&mdash;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8212, Decoded));  %% em dash, U+2014 ISOpub -->
decode_for_xml("&lsquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8216, Decoded));  %% left single quotation mark, U+2018 ISOnum -->
decode_for_xml("&rsquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8217, Decoded));  %% right single quotation mark, U+2019 ISOnum -->
decode_for_xml("&sbquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8218, Decoded));  %% single low-9 quotation mark, U+201A NEW -->
decode_for_xml("&ldquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8220, Decoded));  %% left double quotation mark, U+201C ISOnum -->
decode_for_xml("&rdquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8221, Decoded));  %% right double quotation mark, U+201D ISOnum -->
decode_for_xml("&bdquo;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8222, Decoded));  %% double low-9 quotation mark, U+201E NEW -->
decode_for_xml("&dagger;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8224, Decoded));  %% dagger, U+2020 ISOpub -->
decode_for_xml("&Dagger;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8225, Decoded));  %% double dagger, U+2021 ISOpub -->
decode_for_xml("&permil;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8240, Decoded));  %% per mille sign, U+2030 ISOtech -->
decode_for_xml("&lsaquo;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8249, Decoded));  %% single left-pointing angle quotation mark, U+2039 ISO proposed -->
%% lsaquo is proposed but not yet ISO standardized -->
decode_for_xml("&rsaquo;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8250, Decoded));  %% single right-pointing angle quotation mark, U+203A ISO proposed -->
%% rsaquo is proposed but not yet ISO standardized -->
decode_for_xml("&euro;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8364, Decoded));  %% euro sign, U+20AC NEW -->                                  


%% Mathematical, Greek and Symbolic characters for HTML -->

%% Latin Extended-B -->
decode_for_xml("&fnof;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#402, Decoded));  %% latin small f with hook = function = florin, U+0192 ISOtech -->
%% Greek -->
decode_for_xml("&Alpha;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#913, Decoded));  %% greek capital letter alpha, U+0391 -->
decode_for_xml("&Beta;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#914, Decoded));  %% greek capital letter beta, U+0392 -->
decode_for_xml("&Gamma;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#915, Decoded));  %% greek capital letter gamma, U+0393 ISOgrk3 -->
decode_for_xml("&Delta;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#916, Decoded));  %% greek capital letter delta, U+0394 ISOgrk3 -->
decode_for_xml("&Epsilon;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#917, Decoded));  %% greek capital letter epsilon, U+0395 -->
decode_for_xml("&Zeta;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#918, Decoded));  %% greek capital letter zeta, U+0396 -->
decode_for_xml("&Eta;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#919, Decoded));  %% greek capital letter eta, U+0397 -->
decode_for_xml("&Theta;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#920, Decoded));  %% greek capital letter theta, U+0398 ISOgrk3 -->
decode_for_xml("&Iota;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#921, Decoded));  %% greek capital letter iota, U+0399 -->
decode_for_xml("&Kappa;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#922, Decoded));  %% greek capital letter kappa, U+039A -->
decode_for_xml("&Lambda;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#923, Decoded));  %% greek capital letter lambda, U+039B ISOgrk3 -->
decode_for_xml("&Mu;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#924, Decoded));  %% greek capital letter mu, U+039C -->
decode_for_xml("&Nu;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#925, Decoded));  %% greek capital letter nu, U+039D -->
decode_for_xml("&Xi;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#926, Decoded));  %% greek capital letter xi, U+039E ISOgrk3 -->
decode_for_xml("&Omicron;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#927, Decoded));  %% greek capital letter omicron, U+039F -->
decode_for_xml("&Pi;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#928, Decoded));  %% greek capital letter pi, U+03A0 ISOgrk3 -->
decode_for_xml("&Rho;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#929, Decoded));  %% greek capital letter rho, U+03A1 -->
%% there is no Sigmaf, and no U+03A2 character either -->
decode_for_xml("&Sigma;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#931, Decoded));  %% greek capital letter sigma, U+03A3 ISOgrk3 -->
decode_for_xml("&Tau;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#932, Decoded));  %% greek capital letter tau, U+03A4 -->
decode_for_xml("&Upsilon;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#933, Decoded));  %% greek capital letter upsilon, U+03A5 ISOgrk3 -->
decode_for_xml("&Phi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#934, Decoded));  %% greek capital letter phi, U+03A6 ISOgrk3 -->
decode_for_xml("&Chi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#935, Decoded));  %% greek capital letter chi, U+03A7 -->
decode_for_xml("&Psi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#936, Decoded));  %% greek capital letter psi, U+03A8 ISOgrk3 -->
decode_for_xml("&Omega;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#937, Decoded));  %% greek capital letter omega, U+03A9 ISOgrk3 -->
decode_for_xml("&alpha;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#945, Decoded));  %% greek small letter alpha, U+03B1 ISOgrk3 -->
decode_for_xml("&beta;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#946, Decoded));  %% greek small letter beta, U+03B2 ISOgrk3 -->
decode_for_xml("&gamma;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#947, Decoded));  %% greek small letter gamma, U+03B3 ISOgrk3 -->
decode_for_xml("&delta;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#948, Decoded));  %% greek small letter delta, U+03B4 ISOgrk3 -->
decode_for_xml("&epsilon;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#949, Decoded));  %% greek small letter epsilon, U+03B5 ISOgrk3 -->
decode_for_xml("&zeta;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#950, Decoded));  %% greek small letter zeta, U+03B6 ISOgrk3 -->
decode_for_xml("&eta;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#951, Decoded));  %% greek small letter eta, U+03B7 ISOgrk3 -->
decode_for_xml("&theta;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#952, Decoded));  %% greek small letter theta, U+03B8 ISOgrk3 -->
decode_for_xml("&iota;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#953, Decoded));  %% greek small letter iota, U+03B9 ISOgrk3 -->
decode_for_xml("&kappa;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#954, Decoded));  %% greek small letter kappa, U+03BA ISOgrk3 -->
decode_for_xml("&lambda;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#955, Decoded));  %% greek small letter lambda, U+03BB ISOgrk3 -->
decode_for_xml("&mu;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#956, Decoded));  %% greek small letter mu, U+03BC ISOgrk3 -->
decode_for_xml("&nu;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#957, Decoded));  %% greek small letter nu, U+03BD ISOgrk3 -->
decode_for_xml("&xi;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#958, Decoded));  %% greek small letter xi, U+03BE ISOgrk3 -->
decode_for_xml("&omicron;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#959, Decoded));  %% greek small letter omicron, U+03BF NEW -->
decode_for_xml("&pi;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#960, Decoded));  %% greek small letter pi, U+03C0 ISOgrk3 -->
decode_for_xml("&rho;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#961, Decoded));  %% greek small letter rho, U+03C1 ISOgrk3 -->
decode_for_xml("&sigmaf;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#962, Decoded));  %% greek small letter final sigma, U+03C2 ISOgrk3 -->
decode_for_xml("&sigma;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#963, Decoded));  %% greek small letter sigma, U+03C3 ISOgrk3 -->
decode_for_xml("&tau;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#964, Decoded));  %% greek small letter tau, U+03C4 ISOgrk3 -->
decode_for_xml("&upsilon;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#965, Decoded));  %% greek small letter upsilon, U+03C5 ISOgrk3 -->
decode_for_xml("&phi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#966, Decoded));  %% greek small letter phi, U+03C6 ISOgrk3 -->
decode_for_xml("&chi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#967, Decoded));  %% greek small letter chi, U+03C7 ISOgrk3 -->
decode_for_xml("&psi;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#968, Decoded));  %% greek small letter psi, U+03C8 ISOgrk3 -->
decode_for_xml("&omega;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#969, Decoded));  %% greek small letter omega, U+03C9 ISOgrk3 -->
decode_for_xml("&thetasym;"++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#977, Decoded));  %% greek small letter theta symbol, U+03D1 NEW -->
decode_for_xml("&upsih;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#978, Decoded));  %% greek upsilon with hook symbol, U+03D2 NEW -->
decode_for_xml("&piv;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#982, Decoded));  %% greek pi symbol, U+03D6 ISOgrk3 -->

%% General Punctuation -->
decode_for_xml("&bull;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8226, Decoded));  %% bullet = black small circle, U+2022 ISOpub  -->
%% bullet is NOT the same as bullet operator, U+2219 -->
decode_for_xml("&hellip;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8230, Decoded));  %% horizontal ellipsis = three dot leader, U+2026 ISOpub  -->
decode_for_xml("&prime;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8242, Decoded));  %% prime = minutes = feet, U+2032 ISOtech -->
decode_for_xml("&Prime;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8243, Decoded));  %% double prime = seconds = inches, U+2033 ISOtech -->
decode_for_xml("&oline;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8254, Decoded));  %% overline = spacing overscore, U+203E NEW -->
decode_for_xml("&frasl;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8260, Decoded));  %% fraction slash, U+2044 NEW -->

%% Letterlike Symbols -->
decode_for_xml("&weierp;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8472, Decoded));  %% script capital P = power set = Weierstrass p, U+2118 ISOamso -->
decode_for_xml("&image;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8465, Decoded));  %% blackletter capital I = imaginary part, U+2111 ISOamso -->
decode_for_xml("&real;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8476, Decoded));  %% blackletter capital R = real part symbol, U+211C ISOamso -->
decode_for_xml("&trade;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8482, Decoded));  %% trade mark sign, U+2122 ISOnum -->
decode_for_xml("&alefsym;" ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8501, Decoded));  %% alef symbol = first transfinite cardinal, U+2135 NEW -->
%% alef symbol is NOT the same as hebrew letter alef, U+05D0 although the same glyph could be used to depict both characters -->

%% Arrows -->
decode_for_xml("&larr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8592, Decoded));  %% leftwards arrow, U+2190 ISOnum -->
decode_for_xml("&uarr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8593, Decoded));  %% upwards arrow, U+2191 ISOnum-->
decode_for_xml("&rarr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8594, Decoded));  %% rightwards arrow, U+2192 ISOnum -->
decode_for_xml("&darr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8595, Decoded));  %% downwards arrow, U+2193 ISOnum -->
decode_for_xml("&harr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8596, Decoded));  %% left right arrow, U+2194 ISOamsa -->
decode_for_xml("&crarr;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8629, Decoded));  %% downwards arrow with corner leftwards = carriage return, U+21B5 NEW -->
decode_for_xml("&lArr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8656, Decoded));  %% leftwards double arrow, U+21D0 ISOtech -->
%% ISO 10646 does not say that lArr is the same as the 'is implied by' arrow but also does not have any other character for that function. So ? lArr can be used for 'is implied by' as ISOtech suggests -->
decode_for_xml("&uArr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8657, Decoded));  %% upwards double arrow, U+21D1 ISOamsa -->
decode_for_xml("&rArr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8658, Decoded));  %% rightwards double arrow, U+21D2 ISOtech -->
%% ISO 10646 does not say this is the 'implies' character but does not have another character with this function so ? rArr can be used for 'implies' as ISOtech suggests -->
decode_for_xml("&dArr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8659, Decoded));  %% downwards double arrow, U+21D3 ISOamsa -->
decode_for_xml("&hArr;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8660, Decoded));  %% left right double arrow, U+21D4 ISOamsa -->

%% Mathematical Operators -->
decode_for_xml("&forall;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8704, Decoded));  %% for all, U+2200 ISOtech -->
decode_for_xml("&part;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8706, Decoded));  %% partial differential, U+2202 ISOtech  -->
decode_for_xml("&exist;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8707, Decoded));  %% there exists, U+2203 ISOtech -->
decode_for_xml("&empty;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8709, Decoded));  %% empty set = null set = diameter, U+2205 ISOamso -->
decode_for_xml("&nabla;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8711, Decoded));  %% nabla = backward difference, U+2207 ISOtech -->
decode_for_xml("&isin;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8712, Decoded));  %% element of, U+2208 ISOtech -->
decode_for_xml("&notin;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8713, Decoded));  %% not an element of, U+2209 ISOtech -->
decode_for_xml("&ni;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8715, Decoded));  %% contains as member, U+220B ISOtech -->
%% should there be a more memorable name than 'ni'? -->
decode_for_xml("&prod;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8719, Decoded));  %% n-ary product = product sign, U+220F ISOamsb -->
%% prod is NOT the same character as U+03A0 'greek capital letter pi' though the same glyph might be used for both -->
decode_for_xml("&sum;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8721, Decoded));  %% n-ary sumation, U+2211 ISOamsb -->
%% sum is NOT the same character as U+03A3 'greek capital letter sigma' though the same glyph might be used for both -->
decode_for_xml("&minus;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8722, Decoded));  %% minus sign, U+2212 ISOtech -->
decode_for_xml("&lowast;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8727, Decoded));  %% asterisk operator, U+2217 ISOtech -->
decode_for_xml("&radic;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8730, Decoded));  %% square root = radical sign, U+221A ISOtech -->
decode_for_xml("&prop;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8733, Decoded));  %% proportional to, U+221D ISOtech -->
decode_for_xml("&infin;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8734, Decoded));  %% infinity, U+221E ISOtech -->
decode_for_xml("&ang;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8736, Decoded));  %% angle, U+2220 ISOamso -->
decode_for_xml("&and;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8743, Decoded));  %% logical and = wedge, U+2227 ISOtech -->
decode_for_xml("&or;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8744, Decoded));  %% logical or = vee, U+2228 ISOtech -->
decode_for_xml("&cap;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8745, Decoded));  %% intersection = cap, U+2229 ISOtech -->
decode_for_xml("&cup;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8746, Decoded));  %% union = cup, U+222A ISOtech -->
decode_for_xml("&int;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8747, Decoded));  %% integral, U+222B ISOtech -->
decode_for_xml("&there4;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8756, Decoded));  %% therefore, U+2234 ISOtech -->
decode_for_xml("&sim;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8764, Decoded));  %% tilde operator = varies with = similar to, U+223C ISOtech -->
%% tilde operator is NOT the same character as the tilde, U+007E, although the same glyph might be used to represent both  -->
decode_for_xml("&cong;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8773, Decoded));  %% approximately equal to, U+2245 ISOtech -->
decode_for_xml("&asymp;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8776, Decoded));  %% almost equal to = asymptotic to, U+2248 ISOamsr -->
decode_for_xml("&ne;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8800, Decoded));  %% not equal to, U+2260 ISOtech -->
decode_for_xml("&equiv;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8801, Decoded));  %% identical to, U+2261 ISOtech -->
decode_for_xml("&le;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8804, Decoded));  %% less-than or equal to, U+2264 ISOtech -->
decode_for_xml("&ge;"      ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8805, Decoded));  %% greater-than or equal to, U+2265 ISOtech -->
decode_for_xml("&sub;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8834, Decoded));  %% subset of, U+2282 ISOtech -->
decode_for_xml("&sup;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8835, Decoded));  %% superset of, U+2283 ISOtech -->
%% note that nsup, 'not a superset of, U+2283' is not covered by the Symbol font encoding and is not included. Should it be, for symmetry? It is in ISOamsn  --> 
decode_for_xml("&nsub;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8836, Decoded));  %% not a subset of, U+2284 ISOamsn -->
decode_for_xml("&sube;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8838, Decoded));  %% subset of or equal to, U+2286 ISOtech -->
decode_for_xml("&supe;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8839, Decoded));  %% superset of or equal to, U+2287 ISOtech -->
decode_for_xml("&oplus;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8853, Decoded));  %% circled plus = direct sum, U+2295 ISOamsb -->
decode_for_xml("&otimes;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8855, Decoded));  %% circled times = vector product, U+2297 ISOamsb -->
decode_for_xml("&perp;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8869, Decoded));  %% up tack = orthogonal to = perpendicular, U+22A5 ISOtech -->
decode_for_xml("&sdot;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8901, Decoded));  %% dot operator, U+22C5 ISOamsb -->
%% dot operator is NOT the same character as U+00B7 middle dot -->

%% Miscellaneous Technical -->
decode_for_xml("&lceil;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8968, Decoded));  %% left ceiling = apl upstile, U+2308 ISOamsc  -->
decode_for_xml("&rceil;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8969, Decoded));  %% right ceiling, U+2309 ISOamsc  -->
decode_for_xml("&lfloor;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8970, Decoded));  %% left floor = apl downstile, U+230A ISOamsc  -->
decode_for_xml("&rfloor;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#8971, Decoded));  %% right floor, U+230B ISOamsc  -->
decode_for_xml("&lang;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9001, Decoded));  %% left-pointing angle bracket = bra, U+2329 ISOtech -->
%% lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation mark' -->
decode_for_xml("&rang;"    ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9002, Decoded));  %% right-pointing angle bracket = ket, U+232A ISOtech -->
%% rang is NOT the same character as U+003E 'greater than' or U+203A 'single right-pointing angle quotation mark' -->

%% Geometric Shapes -->
decode_for_xml("&loz;"     ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9674, Decoded));  %% lozenge, U+25CA ISOpub -->

%% Miscellaneous Symbols -->
decode_for_xml("&spades;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9824, Decoded));  %% black spade suit, U+2660 ISOpub -->
%% black here seems to mean filled as opposed to hollow -->
decode_for_xml("&clubs;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9827, Decoded));  %% black club suit = shamrock, U+2663 ISOpub -->
decode_for_xml("&hearts;"  ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9829, Decoded));  %% black heart suit = valentine, U+2665 ISOpub -->
decode_for_xml("&diams;"   ++T, Decoded) -> decode_for_xml(T, concat_as_utf8(10#9830, Decoded));  %% black diamond suit, U+2666 ISOpub -->

decode_for_xml([H|T],           Decoded) -> decode_for_xml(T, [H|Decoded]).
                                                
concat_as_utf8(C, Decoded) when integer(C), C >= 0 ->
    if  C < 128 ->
	    %% 0yyyyyyy
	    [C|Decoded];
        C < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    B1 = 16#C0 + (C bsr 6),
	    B2 = 128 + (C band 16#3F),
            [B2|[B1|Decoded]];
        C < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if  C < 16#D800; C > 16#DFFF, C < 16#FFFE ->
		    B1 = 16#E0 + (C bsr 12),
		    B2 = 128 + ((C bsr 6) band 16#3F),
		    B3 = 128 + (C band 16#3F),
                    [B3|[B2|[B1|Decoded]]]
	    end;
        C < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    B1 = 16#F0 + (C bsr 18),
	    B2 = 128 + ((C bsr 12) band 16#3F),
	    B3 = 128 + ((C bsr 6) band 16#3F),
	    B4 = 128 + (C band 16#3F),
            [B4|[B3|[B2|[B1|Decoded]]]];
        C < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    B1 = 16#F8 + (C bsr 24),
	    B2 = 128 + ((C bsr 18) band 16#3F),
	    B3 = 128 + ((C bsr 12) band 16#3F),
	    B4 = 128 + ((C bsr 6) band 16#3F),
	    B5 = 128 + (C band 16#3F),
            [B5|[B4|[B3|[B2|[B1|Decoded]]]]];
        C < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    B1 = 16#FC + (C bsr 30),
	    B2 = 128 + ((C bsr 24) band 16#3F),
	    B3 = 128 + ((C bsr 18) band 16#3F),
	    B4 = 128 + ((C bsr 12) band 16#3F),
	    B5 = 128 + ((C bsr 6) band 16#3F),
	    B6 = 128 + (C band 16#3F),
            [B6|[B5|[B4|[B3|[B2|[B1|Decoded]]]]]]
    end.
    
    
    
%% reference only:    
decode(EncodedHtml) -> decode(EncodedHtml, []).
decode("&nbsp;"  ++T, Decoded) -> decode(T, [$\040|Decoded]); % space 
decode("&quot;"  ++T, Decoded) -> decode(T, [$\042|Decoded]); % "
decode("&amp;"   ++T, Decoded) -> decode(T, [$\046|Decoded]); % &
decode("&apos;"  ++T, Decoded) -> decode(T, [$\047|Decoded]); % '
decode("&lt;"    ++T, Decoded) -> decode(T, [$\074|Decoded]); % <
decode("&gt;"    ++T, Decoded) -> decode(T, [$\076|Decoded]); % >
decode("&#"      ++T, Decoded) -> 
    {Rest, Char}= match_ascii(T, []),
    decode(Rest, [Char|Decoded]);
decode([H|T],         Decoded) -> decode(T, [H|Decoded]);
decode([],            Decoded) -> lists:reverse(Decoded).

%% @spec string() -> integer
match_ascii(";"  ++T, Ascii) -> {T, list_to_integer(lists:reverse(Ascii))};
match_ascii([H|T],    Ascii) -> match_ascii(T, [H|Ascii]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc create an ets table that contains html entity refs as xmerl rules
%% Usage: xmerl_scan:string(XmlText, [{rules, get_xmerl_rules}])
%% 
-define(TabName, html_xmerl_rules).

-define(CharEntityRefDTD, "
<!-- Character entity set. Typical invocation:
     <!ENTITY % HTMLlat1 PUBLIC
       \"-//W3C//ENTITIES Latin 1//EN//HTML\">
     %HTMLlat1;
-->

<!ENTITY nbsp   \"&#160;\">  <!-- no-break space = non-breaking space, U+00A0 ISOnum -->
<!ENTITY iexcl  \"&#161;\">  <!-- inverted exclamation mark, U+00A1 ISOnum -->
<!ENTITY cent   \"&#162;\">  <!-- cent sign, U+00A2 ISOnum -->
<!ENTITY pound  \"&#163;\">  <!-- pound sign, U+00A3 ISOnum -->
<!ENTITY curren \"&#164;\">  <!-- currency sign, U+00A4 ISOnum -->
<!ENTITY yen    \"&#165;\">  <!-- yen sign = yuan sign, U+00A5 ISOnum -->
<!ENTITY brvbar \"&#166;\">  <!-- broken bar = broken vertical bar, U+00A6 ISOnum -->
<!ENTITY sect   \"&#167;\">  <!-- section sign, U+00A7 ISOnum -->
<!ENTITY uml    \"&#168;\">  <!-- diaeresis = spacing diaeresis, U+00A8 ISOdia -->
<!ENTITY copy   \"&#169;\">  <!-- copyright sign, U+00A9 ISOnum -->
<!ENTITY ordf   \"&#170;\">  <!-- feminine ordinal indicator, U+00AA ISOnum -->
<!ENTITY laquo  \"&#171;\">  <!-- left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
<!ENTITY not    \"&#172;\">  <!-- not sign, U+00AC ISOnum -->
<!ENTITY shy    \"&#173;\">  <!-- soft hyphen = discretionary hyphen, U+00AD ISOnum -->
<!ENTITY reg    \"&#174;\">  <!-- registered sign = registered trade mark sign, U+00AE ISOnum -->                                  
<!ENTITY macr   \"&#175;\">  <!-- macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->                                  
<!ENTITY deg    \"&#176;\">  <!-- degree sign, U+00B0 ISOnum -->
<!ENTITY plusmn \"&#177;\">  <!-- plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->                                
<!ENTITY sup2   \"&#178;\">  <!-- superscript two = superscript digit two = squared, U+00B2 ISOnum -->
<!ENTITY sup3   \"&#179;\">  <!-- superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
<!ENTITY acute  \"&#180;\">  <!-- acute accent = spacing acute, U+00B4 ISOdia -->
<!ENTITY micro  \"&#181;\">  <!-- micro sign, U+00B5 ISOnum -->
<!ENTITY para   \"&#182;\">  <!-- pilcrow sign = paragraph sign, U+00B6 ISOnum -->
<!ENTITY middot \"&#183;\">  <!-- middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
<!ENTITY cedil  \"&#184;\">  <!-- cedilla = spacing cedilla, U+00B8 ISOdia -->
<!ENTITY sup1   \"&#185;\">  <!-- superscript one = superscript digit one, U+00B9 ISOnum -->
<!ENTITY ordm   \"&#186;\">  <!-- masculine ordinal indicator, U+00BA ISOnum -->
<!ENTITY raquo  \"&#187;\">  <!-- right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
<!ENTITY frac14 \"&#188;\">  <!-- vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
<!ENTITY frac12 \"&#189;\">  <!-- vulgar fraction one half = fraction one half, U+00BD ISOnum -->
<!ENTITY frac34 \"&#190;\">  <!-- vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
<!ENTITY iquest \"&#191;\">  <!-- inverted question mark = turned question mark, U+00BF ISOnum -->
<!ENTITY Agrave \"&#192;\">  <!-- latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
<!ENTITY Aacute \"&#193;\">  <!-- latin capital letter A with acute, U+00C1 ISOlat1 -->
<!ENTITY Acirc  \"&#194;\">  <!-- latin capital letter A with circumflex, U+00C2 ISOlat1 -->
<!ENTITY Atilde \"&#195;\">  <!-- latin capital letter A with tilde, U+00C3 ISOlat1 -->
<!ENTITY Auml   \"&#196;\">  <!-- latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
<!ENTITY Aring  \"&#197;\">  <!-- latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
<!ENTITY AElig  \"&#198;\">  <!-- latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
<!ENTITY Ccedil \"&#199;\">  <!-- latin capital letter C with cedilla, U+00C7 ISOlat1 -->
<!ENTITY Egrave \"&#200;\">  <!-- latin capital letter E with grave, U+00C8 ISOlat1 -->
<!ENTITY Eacute \"&#201;\">  <!-- latin capital letter E with acute, U+00C9 ISOlat1 -->
<!ENTITY Ecirc  \"&#202;\">  <!-- latin capital letter E with circumflex, U+00CA ISOlat1 -->
<!ENTITY Euml   \"&#203;\">  <!-- latin capital letter E with diaeresis, U+00CB ISOlat1 -->
<!ENTITY Igrave \"&#204;\">  <!-- latin capital letter I with grave, U+00CC ISOlat1 -->
<!ENTITY Iacute \"&#205;\">  <!-- latin capital letter I with acute, U+00CD ISOlat1 -->
<!ENTITY Icirc  \"&#206;\">  <!-- latin capital letter I with circumflex, U+00CE ISOlat1 -->
<!ENTITY Iuml   \"&#207;\">  <!-- latin capital letter I with diaeresis, U+00CF ISOlat1 -->
<!ENTITY ETH    \"&#208;\">  <!-- latin capital letter ETH, U+00D0 ISOlat1 -->
<!ENTITY Ntilde \"&#209;\">  <!-- latin capital letter N with tilde, U+00D1 ISOlat1 -->
<!ENTITY Ograve \"&#210;\">  <!-- latin capital letter O with grave, U+00D2 ISOlat1 -->
<!ENTITY Oacute \"&#211;\">  <!-- latin capital letter O with acute, U+00D3 ISOlat1 -->
<!ENTITY Ocirc  \"&#212;\">  <!-- latin capital letter O with circumflex, U+00D4 ISOlat1 -->
<!ENTITY Otilde \"&#213;\">  <!-- latin capital letter O with tilde, U+00D5 ISOlat1 -->
<!ENTITY Ouml   \"&#214;\">  <!-- latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
<!ENTITY times  \"&#215;\">  <!-- multiplication sign, U+00D7 ISOnum -->
<!ENTITY Oslash \"&#216;\">  <!-- latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
<!ENTITY Ugrave \"&#217;\">  <!-- latin capital letter U with grave, U+00D9 ISOlat1 -->
<!ENTITY Uacute \"&#218;\">  <!-- latin capital letter U with acute, U+00DA ISOlat1 -->
<!ENTITY Ucirc  \"&#219;\">  <!-- latin capital letter U with circumflex, U+00DB ISOlat1 -->
<!ENTITY Uuml   \"&#220;\">  <!-- latin capital letter U with diaeresis, U+00DC ISOlat1 -->
<!ENTITY Yacute \"&#221;\">  <!-- latin capital letter Y with acute, U+00DD ISOlat1 -->
<!ENTITY THORN  \"&#222;\">  <!-- latin capital letter THORN, U+00DE ISOlat1 -->
<!ENTITY szlig  \"&#223;\">  <!-- latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
<!ENTITY agrave \"&#224;\">  <!-- latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
<!ENTITY aacute \"&#225;\">  <!-- latin small letter a with acute, U+00E1 ISOlat1 -->
<!ENTITY acirc  \"&#226;\">  <!-- latin small letter a with circumflex, U+00E2 ISOlat1 -->
<!ENTITY atilde \"&#227;\">  <!-- latin small letter a with tilde, U+00E3 ISOlat1 -->
<!ENTITY auml   \"&#228;\">  <!-- latin small letter a with diaeresis, U+00E4 ISOlat1 -->
<!ENTITY aring  \"&#229;\">  <!-- latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
<!ENTITY aelig  \"&#230;\">  <!-- latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
<!ENTITY ccedil \"&#231;\">  <!-- latin small letter c with cedilla, U+00E7 ISOlat1 -->
<!ENTITY egrave \"&#232;\">  <!-- latin small letter e with grave, U+00E8 ISOlat1 -->
<!ENTITY eacute \"&#233;\">  <!-- latin small letter e with acute, U+00E9 ISOlat1 -->
<!ENTITY ecirc  \"&#234;\">  <!-- latin small letter e with circumflex, U+00EA ISOlat1 -->
<!ENTITY euml   \"&#235;\">  <!-- latin small letter e with diaeresis, U+00EB ISOlat1 -->
<!ENTITY igrave \"&#236;\">  <!-- latin small letter i with grave, U+00EC ISOlat1 -->
<!ENTITY iacute \"&#237;\">  <!-- latin small letter i with acute, U+00ED ISOlat1 -->
<!ENTITY icirc  \"&#238;\">  <!-- latin small letter i with circumflex, U+00EE ISOlat1 -->
<!ENTITY iuml   \"&#239;\">  <!-- latin small letter i with diaeresis, U+00EF ISOlat1 -->
<!ENTITY eth    \"&#240;\">  <!-- latin small letter eth, U+00F0 ISOlat1 -->
<!ENTITY ntilde \"&#241;\">  <!-- latin small letter n with tilde, U+00F1 ISOlat1 -->
<!ENTITY ograve \"&#242;\">  <!-- latin small letter o with grave, U+00F2 ISOlat1 -->
<!ENTITY oacute \"&#243;\">  <!-- latin small letter o with acute, U+00F3 ISOlat1 -->
<!ENTITY ocirc  \"&#244;\">  <!-- latin small letter o with circumflex, U+00F4 ISOlat1 -->
<!ENTITY otilde \"&#245;\">  <!-- latin small letter o with tilde, U+00F5 ISOlat1 -->
<!ENTITY ouml   \"&#246;\">  <!-- latin small letter o with diaeresis, U+00F6 ISOlat1 -->
<!ENTITY divide \"&#247;\">  <!-- division sign, U+00F7 ISOnum -->
<!ENTITY oslash \"&#248;\">  <!-- latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1 -->
<!ENTITY ugrave \"&#249;\">  <!-- latin small letter u with grave, U+00F9 ISOlat1 -->
<!ENTITY uacute \"&#250;\">  <!-- latin small letter u with acute, U+00FA ISOlat1 -->
<!ENTITY ucirc  \"&#251;\">  <!-- latin small letter u with circumflex, U+00FB ISOlat1 -->
<!ENTITY uuml   \"&#252;\">  <!-- latin small letter u with diaeresis, U+00FC ISOlat1 -->
<!ENTITY yacute \"&#253;\">  <!-- latin small letter y with acute, U+00FD ISOlat1 -->
<!ENTITY thorn  \"&#254;\">  <!-- latin small letter thorn, U+00FE ISOlat1 -->
<!ENTITY yuml   \"&#255;\">  <!-- latin small letter y with diaeresis, U+00FF ISOlat1 -->


<!-- Special characters for HTML -->

<!-- Character entity set. Typical invocation:
     <!ENTITY % HTMLspecial PUBLIC
       \"-//W3C//ENTITIES Special//EN//HTML\">
     %HTMLspecial; -->

<!-- C0 Controls and Basic Latin -->
<!-- These 4 chars are keep for xml, so we just ignore them. -->
<!--ENTITY quot    \"&#34;\"-->  <!-- quotation mark = APL quote, U+0022 ISOnum -->
<!--ENTITY amp     \"&#38;\"-->  <!-- ampersand, U+0026 ISOnum -->
<!--ENTITY lt      \"&#60;\"-->  <!-- less-than sign, U+003C ISOnum -->
<!--ENTITY gt      \"&#62;\"-->  <!-- greater-than sign, U+003E ISOnum -->

<!-- Latin Extended-A -->
<!ENTITY OElig   \"&#338;\">  <!-- latin capital ligature OE, U+0152 ISOlat2 -->
<!ENTITY oelig   \"&#339;\">  <!-- latin small ligature oe, U+0153 ISOlat2 -->
<!-- ligature is a misnomer, this is a separate character in some languages -->
<!ENTITY Scaron  \"&#352;\">  <!-- latin capital letter S with caron, U+0160 ISOlat2 -->
<!ENTITY scaron  \"&#353;\">  <!-- latin small letter s with caron, U+0161 ISOlat2 -->
<!ENTITY Yuml    \"&#376;\">  <!-- latin capital letter Y with diaeresis, U+0178 ISOlat2 -->

<!-- Spacing Modifier Letters -->
<!ENTITY circ    \"&#710;\">  <!-- modifier letter circumflex accent, U+02C6 ISOpub -->
<!ENTITY tilde   \"&#732;\">  <!-- small tilde, U+02DC ISOdia -->

<!-- General Punctuation -->
<!ENTITY ensp    \"&#8194;\">  <!-- en space, U+2002 ISOpub -->
<!ENTITY emsp    \"&#8195;\">  <!-- em space, U+2003 ISOpub -->
<!ENTITY thinsp  \"&#8201;\">  <!-- thin space, U+2009 ISOpub -->
<!ENTITY zwnj    \"&#8204;\">  <!-- zero width non-joiner, U+200C NEW RFC 2070 -->
<!ENTITY zwj     \"&#8205;\">  <!-- zero width joiner, U+200D NEW RFC 2070 -->
<!ENTITY lrm     \"&#8206;\">  <!-- left-to-right mark, U+200E NEW RFC 2070 -->
<!ENTITY rlm     \"&#8207;\">  <!-- right-to-left mark, U+200F NEW RFC 2070 -->
<!ENTITY ndash   \"&#8211;\">  <!-- en dash, U+2013 ISOpub -->
<!ENTITY mdash   \"&#8212;\">  <!-- em dash, U+2014 ISOpub -->
<!ENTITY lsquo   \"&#8216;\">  <!-- left single quotation mark, U+2018 ISOnum -->
<!ENTITY rsquo   \"&#8217;\">  <!-- right single quotation mark, U+2019 ISOnum -->
<!ENTITY sbquo   \"&#8218;\">  <!-- single low-9 quotation mark, U+201A NEW -->
<!ENTITY ldquo   \"&#8220;\">  <!-- left double quotation mark, U+201C ISOnum -->
<!ENTITY rdquo   \"&#8221;\">  <!-- right double quotation mark, U+201D ISOnum -->
<!ENTITY bdquo   \"&#8222;\">  <!-- double low-9 quotation mark, U+201E NEW -->
<!ENTITY dagger  \"&#8224;\">  <!-- dagger, U+2020 ISOpub -->
<!ENTITY Dagger  \"&#8225;\">  <!-- double dagger, U+2021 ISOpub -->
<!ENTITY permil  \"&#8240;\">  <!-- per mille sign, U+2030 ISOtech -->
<!ENTITY lsaquo  \"&#8249;\">  <!-- single left-pointing angle quotation mark, U+2039 ISO proposed -->
<!-- lsaquo is proposed but not yet ISO standardized -->
<!ENTITY rsaquo  \"&#8250;\">  <!-- single right-pointing angle quotation mark, U+203A ISO proposed -->
<!-- rsaquo is proposed but not yet ISO standardized -->
<!ENTITY euro   \"&#8364;\">  <!-- euro sign, U+20AC NEW -->                                  


<!-- Mathematical, Greek and Symbolic characters for HTML -->

<!-- Character entity set. Typical invocation:
     <!ENTITY % HTMLsymbol PUBLIC
       \"-//W3C//ENTITIES Symbols//EN//HTML\">
     %HTMLsymbol; -->

<!-- Latin Extended-B -->
<!ENTITY fnof     \"&#402;\">  <!-- latin small f with hook = function = florin, U+0192 ISOtech -->

<!-- Greek -->
<!ENTITY Alpha    \"&#913;\">  <!-- greek capital letter alpha, U+0391 -->
<!ENTITY Beta     \"&#914;\">  <!-- greek capital letter beta, U+0392 -->
<!ENTITY Gamma    \"&#915;\">  <!-- greek capital letter gamma, U+0393 ISOgrk3 -->
<!ENTITY Delta    \"&#916;\">  <!-- greek capital letter delta, U+0394 ISOgrk3 -->
<!ENTITY Epsilon  \"&#917;\">  <!-- greek capital letter epsilon, U+0395 -->
<!ENTITY Zeta     \"&#918;\">  <!-- greek capital letter zeta, U+0396 -->
<!ENTITY Eta      \"&#919;\">  <!-- greek capital letter eta, U+0397 -->
<!ENTITY Theta    \"&#920;\">  <!-- greek capital letter theta, U+0398 ISOgrk3 -->
<!ENTITY Iota     \"&#921;\">  <!-- greek capital letter iota, U+0399 -->
<!ENTITY Kappa    \"&#922;\">  <!-- greek capital letter kappa, U+039A -->
<!ENTITY Lambda   \"&#923;\">  <!-- greek capital letter lambda, U+039B ISOgrk3 -->
<!ENTITY Mu       \"&#924;\">  <!-- greek capital letter mu, U+039C -->
<!ENTITY Nu       \"&#925;\">  <!-- greek capital letter nu, U+039D -->
<!ENTITY Xi       \"&#926;\">  <!-- greek capital letter xi, U+039E ISOgrk3 -->
<!ENTITY Omicron  \"&#927;\">  <!-- greek capital letter omicron, U+039F -->
<!ENTITY Pi       \"&#928;\">  <!-- greek capital letter pi, U+03A0 ISOgrk3 -->
<!ENTITY Rho      \"&#929;\">  <!-- greek capital letter rho, U+03A1 -->
<!-- there is no Sigmaf, and no U+03A2 character either -->
<!ENTITY Sigma    \"&#931;\">  <!-- greek capital letter sigma, U+03A3 ISOgrk3 -->
<!ENTITY Tau      \"&#932;\">  <!-- greek capital letter tau, U+03A4 -->
<!ENTITY Upsilon  \"&#933;\">  <!-- greek capital letter upsilon, U+03A5 ISOgrk3 -->
<!ENTITY Phi      \"&#934;\">  <!-- greek capital letter phi, U+03A6 ISOgrk3 -->
<!ENTITY Chi      \"&#935;\">  <!-- greek capital letter chi, U+03A7 -->
<!ENTITY Psi      \"&#936;\">  <!-- greek capital letter psi, U+03A8 ISOgrk3 -->
<!ENTITY Omega    \"&#937;\">  <!-- greek capital letter omega, U+03A9 ISOgrk3 -->

<!ENTITY alpha    \"&#945;\">  <!-- greek small letter alpha, U+03B1 ISOgrk3 -->
<!ENTITY beta     \"&#946;\">  <!-- greek small letter beta, U+03B2 ISOgrk3 -->
<!ENTITY gamma    \"&#947;\">  <!-- greek small letter gamma, U+03B3 ISOgrk3 -->
<!ENTITY delta    \"&#948;\">  <!-- greek small letter delta, U+03B4 ISOgrk3 -->
<!ENTITY epsilon  \"&#949;\">  <!-- greek small letter epsilon, U+03B5 ISOgrk3 -->
<!ENTITY zeta     \"&#950;\">  <!-- greek small letter zeta, U+03B6 ISOgrk3 -->
<!ENTITY eta      \"&#951;\">  <!-- greek small letter eta, U+03B7 ISOgrk3 -->
<!ENTITY theta    \"&#952;\">  <!-- greek small letter theta, U+03B8 ISOgrk3 -->
<!ENTITY iota     \"&#953;\">  <!-- greek small letter iota, U+03B9 ISOgrk3 -->
<!ENTITY kappa    \"&#954;\">  <!-- greek small letter kappa, U+03BA ISOgrk3 -->
<!ENTITY lambda   \"&#955;\">  <!-- greek small letter lambda, U+03BB ISOgrk3 -->
<!ENTITY mu       \"&#956;\">  <!-- greek small letter mu, U+03BC ISOgrk3 -->
<!ENTITY nu       \"&#957;\">  <!-- greek small letter nu, U+03BD ISOgrk3 -->
<!ENTITY xi       \"&#958;\">  <!-- greek small letter xi, U+03BE ISOgrk3 -->
<!ENTITY omicron  \"&#959;\">  <!-- greek small letter omicron, U+03BF NEW -->
<!ENTITY pi       \"&#960;\">  <!-- greek small letter pi, U+03C0 ISOgrk3 -->
<!ENTITY rho      \"&#961;\">  <!-- greek small letter rho, U+03C1 ISOgrk3 -->
<!ENTITY sigmaf   \"&#962;\">  <!-- greek small letter final sigma, U+03C2 ISOgrk3 -->
<!ENTITY sigma    \"&#963;\">  <!-- greek small letter sigma, U+03C3 ISOgrk3 -->
<!ENTITY tau      \"&#964;\">  <!-- greek small letter tau, U+03C4 ISOgrk3 -->
<!ENTITY upsilon  \"&#965;\">  <!-- greek small letter upsilon, U+03C5 ISOgrk3 -->
<!ENTITY phi      \"&#966;\">  <!-- greek small letter phi, U+03C6 ISOgrk3 -->
<!ENTITY chi      \"&#967;\">  <!-- greek small letter chi, U+03C7 ISOgrk3 -->
<!ENTITY psi      \"&#968;\">  <!-- greek small letter psi, U+03C8 ISOgrk3 -->
<!ENTITY omega    \"&#969;\">  <!-- greek small letter omega, U+03C9 ISOgrk3 -->
<!ENTITY thetasym \"&#977;\">  <!-- greek small letter theta symbol, U+03D1 NEW -->
<!ENTITY upsih    \"&#978;\">  <!-- greek upsilon with hook symbol, U+03D2 NEW -->
<!ENTITY piv      \"&#982;\">  <!-- greek pi symbol, U+03D6 ISOgrk3 -->

<!-- General Punctuation -->
<!ENTITY bull     \"&#8226;\">  <!-- bullet = black small circle, U+2022 ISOpub  -->
<!-- bullet is NOT the same as bullet operator, U+2219 -->
<!ENTITY hellip   \"&#8230;\">  <!-- horizontal ellipsis = three dot leader, U+2026 ISOpub  -->
<!ENTITY prime    \"&#8242;\">  <!-- prime = minutes = feet, U+2032 ISOtech -->
<!ENTITY Prime    \"&#8243;\">  <!-- double prime = seconds = inches, U+2033 ISOtech -->
<!ENTITY oline    \"&#8254;\">  <!-- overline = spacing overscore, U+203E NEW -->
<!ENTITY frasl    \"&#8260;\">  <!-- fraction slash, U+2044 NEW -->

<!-- Letterlike Symbols -->
<!ENTITY weierp   \"&#8472;\">  <!-- script capital P = power set = Weierstrass p, U+2118 ISOamso -->
<!ENTITY image    \"&#8465;\">  <!-- blackletter capital I = imaginary part, U+2111 ISOamso -->
<!ENTITY real     \"&#8476;\">  <!-- blackletter capital R = real part symbol, U+211C ISOamso -->
<!ENTITY trade    \"&#8482;\">  <!-- trade mark sign, U+2122 ISOnum -->
<!ENTITY alefsym  \"&#8501;\">  <!-- alef symbol = first transfinite cardinal, U+2135 NEW -->
<!-- alef symbol is NOT the same as hebrew letter alef, U+05D0 although 
    the same glyph could be used to depict both characters -->

<!-- Arrows -->
<!ENTITY larr     \"&#8592;\">  <!-- leftwards arrow, U+2190 ISOnum -->
<!ENTITY uarr     \"&#8593;\">  <!-- upwards arrow, U+2191 ISOnum-->
<!ENTITY rarr     \"&#8594;\">  <!-- rightwards arrow, U+2192 ISOnum -->
<!ENTITY darr     \"&#8595;\">  <!-- downwards arrow, U+2193 ISOnum -->
<!ENTITY harr     \"&#8596;\">  <!-- left right arrow, U+2194 ISOamsa -->
<!ENTITY crarr    \"&#8629;\">  <!-- downwards arrow with corner leftwards = carriage return, U+21B5 NEW -->
<!ENTITY lArr     \"&#8656;\">  <!-- leftwards double arrow, U+21D0 ISOtech -->
<!-- ISO 10646 does not say that lArr is the same as the 'is implied by' arrow
    but also does not have any other character for that function. So ? lArr can
    be used for 'is implied by' as ISOtech suggests -->
<!ENTITY uArr     \"&#8657;\">  <!-- upwards double arrow, U+21D1 ISOamsa -->
<!ENTITY rArr     \"&#8658;\">  <!-- rightwards double arrow, U+21D2 ISOtech -->
<!-- ISO 10646 does not say this is the 'implies' character but does not have 
     another character with this function so ?
     rArr can be used for 'implies' as ISOtech suggests -->
<!ENTITY dArr     \"&#8659;\">  <!-- downwards double arrow, U+21D3 ISOamsa -->
<!ENTITY hArr     \"&#8660;\">  <!-- left right double arrow, U+21D4 ISOamsa -->

<!-- Mathematical Operators -->
<!ENTITY forall   \"&#8704;\">  <!-- for all, U+2200 ISOtech -->
<!ENTITY part     \"&#8706;\">  <!-- partial differential, U+2202 ISOtech  -->
<!ENTITY exist    \"&#8707;\">  <!-- there exists, U+2203 ISOtech -->
<!ENTITY empty    \"&#8709;\">  <!-- empty set = null set = diameter, U+2205 ISOamso -->
<!ENTITY nabla    \"&#8711;\">  <!-- nabla = backward difference, U+2207 ISOtech -->
<!ENTITY isin     \"&#8712;\">  <!-- element of, U+2208 ISOtech -->
<!ENTITY notin    \"&#8713;\">  <!-- not an element of, U+2209 ISOtech -->
<!ENTITY ni       \"&#8715;\">  <!-- contains as member, U+220B ISOtech -->
<!-- should there be a more memorable name than 'ni'? -->
<!ENTITY prod     \"&#8719;\">  <!-- n-ary product = product sign, U+220F ISOamsb -->
<!-- prod is NOT the same character as U+03A0 'greek capital letter pi' though
     the same glyph might be used for both -->
<!ENTITY sum      \"&#8721;\">  <!-- n-ary sumation, U+2211 ISOamsb -->
<!-- sum is NOT the same character as U+03A3 'greek capital letter sigma'
     though the same glyph might be used for both -->
<!ENTITY minus    \"&#8722;\">  <!-- minus sign, U+2212 ISOtech -->
<!ENTITY lowast   \"&#8727;\">  <!-- asterisk operator, U+2217 ISOtech -->
<!ENTITY radic    \"&#8730;\">  <!-- square root = radical sign, U+221A ISOtech -->
<!ENTITY prop     \"&#8733;\">  <!-- proportional to, U+221D ISOtech -->
<!ENTITY infin    \"&#8734;\">  <!-- infinity, U+221E ISOtech -->
<!ENTITY ang      \"&#8736;\">  <!-- angle, U+2220 ISOamso -->
<!ENTITY and      \"&#8743;\">  <!-- logical and = wedge, U+2227 ISOtech -->
<!ENTITY or       \"&#8744;\">  <!-- logical or = vee, U+2228 ISOtech -->
<!ENTITY cap      \"&#8745;\">  <!-- intersection = cap, U+2229 ISOtech -->
<!ENTITY cup      \"&#8746;\">  <!-- union = cup, U+222A ISOtech -->
<!ENTITY int      \"&#8747;\">  <!-- integral, U+222B ISOtech -->
<!ENTITY there4   \"&#8756;\">  <!-- therefore, U+2234 ISOtech -->
<!ENTITY sim      \"&#8764;\">  <!-- tilde operator = varies with = similar to, U+223C ISOtech -->
<!-- tilde operator is NOT the same character as the tilde, U+007E,
     although the same glyph might be used to represent both  -->
<!ENTITY cong     \"&#8773;\">  <!-- approximately equal to, U+2245 ISOtech -->
<!ENTITY asymp    \"&#8776;\">  <!-- almost equal to = asymptotic to, U+2248 ISOamsr -->
<!ENTITY ne       \"&#8800;\">  <!-- not equal to, U+2260 ISOtech -->
<!ENTITY equiv    \"&#8801;\">  <!-- identical to, U+2261 ISOtech -->
<!ENTITY le       \"&#8804;\">  <!-- less-than or equal to, U+2264 ISOtech -->
<!ENTITY ge       \"&#8805;\">  <!-- greater-than or equal to, U+2265 ISOtech -->
<!ENTITY sub      \"&#8834;\">  <!-- subset of, U+2282 ISOtech -->
<!ENTITY sup      \"&#8835;\">  <!-- superset of, U+2283 ISOtech -->
<!-- note that nsup, 'not a superset of, U+2283' is not covered by the Symbol 
     font encoding and is not included. Should it be, for symmetry?
     It is in ISOamsn  --> 
<!ENTITY nsub     \"&#8836;\">  <!-- not a subset of, U+2284 ISOamsn -->
<!ENTITY sube     \"&#8838;\">  <!-- subset of or equal to, U+2286 ISOtech -->
<!ENTITY supe     \"&#8839;\">  <!-- superset of or equal to, U+2287 ISOtech -->
<!ENTITY oplus    \"&#8853;\">  <!-- circled plus = direct sum, U+2295 ISOamsb -->
<!ENTITY otimes   \"&#8855;\">  <!-- circled times = vector product, U+2297 ISOamsb -->
<!ENTITY perp     \"&#8869;\">  <!-- up tack = orthogonal to = perpendicular, U+22A5 ISOtech -->
<!ENTITY sdot     \"&#8901;\">  <!-- dot operator, U+22C5 ISOamsb -->
<!-- dot operator is NOT the same character as U+00B7 middle dot -->

<!-- Miscellaneous Technical -->
<!ENTITY lceil    \"&#8968;\">  <!-- left ceiling = apl upstile, U+2308 ISOamsc  -->
<!ENTITY rceil    \"&#8969;\">  <!-- right ceiling, U+2309 ISOamsc  -->
<!ENTITY lfloor   \"&#8970;\">  <!-- left floor = apl downstile, U+230A ISOamsc  -->
<!ENTITY rfloor   \"&#8971;\">  <!-- right floor, U+230B ISOamsc  -->
<!ENTITY lang     \"&#9001;\">  <!-- left-pointing angle bracket = bra, U+2329 ISOtech -->
<!-- lang is NOT the same character as U+003C 'less than' 
     or U+2039 'single left-pointing angle quotation mark' -->
<!ENTITY rang     \"&#9002;\">  <!-- right-pointing angle bracket = ket, U+232A ISOtech -->
<!-- rang is NOT the same character as U+003E 'greater than' 
     or U+203A 'single right-pointing angle quotation mark' -->

<!-- Geometric Shapes -->
<!ENTITY loz      \"&#9674;\">  <!-- lozenge, U+25CA ISOpub -->

<!-- Miscellaneous Symbols -->
<!ENTITY spades   \"&#9824;\">  <!-- black spade suit, U+2660 ISOpub -->
<!-- black here seems to mean filled as opposed to hollow -->
<!ENTITY clubs    \"&#9827;\">  <!-- black club suit = shamrock, U+2663 ISOpub -->
<!ENTITY hearts   \"&#9829;\">  <!-- black heart suit = valentine, U+2665 ISOpub -->
<!ENTITY diams    \"&#9830;\">  <!-- black diamond suit, U+2666 ISOpub -->

").                                  
                                                

get_xmerl_rules() ->
    %% The ets table will be deleted if the process that create this ets table quit
    %% even the table is created as public. 
    %% To enable long-living ets table, should create a seperate long-living process, 
    %% and create ets table from this process.
    case ets:info(?TabName) of
        undefined -> 
            init_xmerl_rules(); 
        _ ->
            ?TabName 
    end.
                                                
init_xmerl_rules() ->
    Tab = ets:new(?TabName, [set, public, named_table]),
    catch xmerl_scan:string("<?xml version='1.0' encoding='UTF-8'?>
 	                     <!DOCTYPE html_xmerl_rules SYSTEM 'CharEntityRef.DTD'>
 	                     <html_xmerl_rules></html_xmerl_rules>",
 	                    [{fetch_fun, dtd_fetch_fun()},
 	                     {rules, ?TabName}]),
    Tab.

dtd_fetch_fun() ->
    fun(_, State) ->
 	{ok, {string, ?CharEntityRefDTD}, State} 
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tests    
xmerl_rules_test_() ->
    {ok, XmlBin} = file:read_file("../pole/data/fodor.xml"),
    XmlText = binary_to_list(XmlBin),
    Before = now(),
    Rules = get_xmerl_rules(),
    io:fwrite(user, "Info of rules table: ~p~n", [ets:info(?TabName)]),
    {DocElement, _Rest} = xmerl_scan:string(XmlText, [{rules, Rules}]),
    ParsingTime = timer:now_diff(now(), Before),

    %io:fwrite(user, "DocElement: ~p~n", [DocElement]),
    io:fwrite(user, "Timer Parsing: ~B~n", [ParsingTime]).

decode_for_xml_test_() ->
    {ok, XmlBin} = file:read_file("../pole/data/fodor.xml"),
    XmlText = binary_to_list(XmlBin),
    XmlText1 = decode_for_xml(XmlText),
    Before = now(),
    {DocElement, _Rest} = xmerl_scan:string(XmlText1),
    ParsingTime = timer:now_diff(now(), Before),

    %io:fwrite(user, "DocElement: ~p~n", [DocElement]),
    io:fwrite(user, "Timer Parsing: ~B~n", [ParsingTime]).

