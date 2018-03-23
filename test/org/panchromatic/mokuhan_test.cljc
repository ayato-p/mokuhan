(ns org.panchromatic.mokuhan-test
  (:require [clojure.test :as t]
            [clojure.walk :as walk]
            [org.panchromatic.mokuhan :as sut]
            [org.panchromatic.mokuhan.test-helper :as h :include-macros true]))

;; (h/generate-test-cases-from-spec comments)

(t/deftest comments-test
  (t/testing
      "Inline / Comment blocks should be removed from the template."
    (t/is
     (=
      "1234567890"
      (sut/render "12345{{! Comment Block! }}67890" {} {}))))
  (t/testing
      "Multiline / Multiline comments should be permitted."
    (t/is
     (=
      "1234567890\n"
      (sut/render
       "12345{{!\n  This is a\n  multi-line comment...\n}}67890\n"
       {}
       {}))))
  (t/testing
      "Standalone / All standalone comment lines should be removed."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render "Begin.\n{{! Comment Block! }}\nEnd.\n" {} {}))))
  (t/testing
      "Indented Standalone / All standalone comment lines should be removed."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render
       "Begin.\n  {{! Indented Comment Block! }}\nEnd.\n"
       {}
       {}))))
  (t/testing
      "Standalone Line Endings / \"\\r\\n\" should be considered a newline for standalone tags."
    (t/is
     (=
      "|\r\n|"
      (sut/render "|\r\n{{! Standalone Comment }}\r\n|" {} {}))))
  (t/testing
      "Standalone Without Previous Line / Standalone tags should not require a newline to precede them."
    (t/is
     (= "!" (sut/render "  {{! I'm Still Standalone }}\n!" {} {}))))
  (t/testing
      "Standalone Without Newline / Standalone tags should not require a newline to follow them."
    (t/is
     (= "!\n" (sut/render "!\n  {{! I'm Still Standalone }}" {} {}))))
  (t/testing
      "Multiline Standalone / All standalone comment lines should be removed."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render
       "Begin.\n{{!\nSomething's going on here...\n}}\nEnd.\n"
       {}
       {}))))
  (t/testing
      "Indented Multiline Standalone / All standalone comment lines should be removed."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render
       "Begin.\n  {{!\n    Something's going on here...\n  }}\nEnd.\n"
       {}
       {}))))
  (t/testing
      "Indented Inline / Inline comments should not strip whitespace"
    (t/is (= "  12 \n" (sut/render "  12 {{! 34 }}\n" {} {}))))
  (t/testing
      "Surrounding Whitespace / Comment removal should preserve surrounding whitespace."
    (t/is
     (=
      "12345  67890"
      (sut/render "12345 {{! Comment Block! }} 67890" {} {})))))

;; (h/generate-test-cases-from-spec delimiters)

(t/deftest delimiters-test
  (t/testing
      "Pair Behavior / The equals sign (used on both sides) should permit delimiter changes."
    (t/is
     (=
      "(Hey!)"
      (sut/render "{{=<% %>=}}(<%text%>)" {:text "Hey!"} {}))))
  (t/testing
      "Special Characters / Characters with special meaning regexen should be valid delimiters."
    (t/is
     (=
      "(It worked!)"
      (sut/render "({{=[ ]=}}[text])" {:text "It worked!"} {}))))
  (t/testing
      "Sections / Delimiters set outside sections should persist."
    (t/is
     (=
      "[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n"
      (sut/render
       "[\n{{#section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|#section|\n  {{data}}\n  |data|\n|/section|\n]\n"
       {:section true, :data "I got interpolated."}
       {}))))
  (t/testing
      "Inverted Sections / Delimiters set outside inverted sections should persist."
    (t/is
     (=
      "[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n"
      (sut/render
       "[\n{{^section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|^section|\n  {{data}}\n  |data|\n|/section|\n]\n"
       {:section false, :data "I got interpolated."}
       {}))))
  (t/testing
      "Partial Inheritence / Delimiters set in a parent template should not affect a partial."
    (t/is
     (=
      "[ .yes. ]\n[ .yes. ]\n"
      (sut/render
       "[ {{>include}} ]\n{{= | | =}}\n[ |>include| ]\n"
       {:value "yes"}
       {:partials {:include ".{{value}}."}}))))
  (t/testing
      "Post-Partial Behavior / Delimiters set in a partial should not affect the parent template."
    (t/is
     (=
      "[ .yes.  .yes. ]\n[ .yes.  .|value|. ]\n"
      (sut/render
       "[ {{>include}} ]\n[ .{{value}}.  .|value|. ]\n"
       {:value "yes"}
       {:partials
        {:include ".{{value}}. {{= | | =}} .|value|."}}))))
  (t/testing
      "Surrounding Whitespace / Surrounding whitespace should be left untouched."
    (t/is (= "|  |" (sut/render "| {{=@ @=}} |" {} {}))))
  (t/testing
      "Outlying Whitespace (Inline) / Whitespace should be left untouched."
    (t/is (= " | \n" (sut/render " | {{=@ @=}}\n" {} {}))))
  (t/testing
      "Standalone Tag / Standalone lines should be removed from the template."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render "Begin.\n{{=@ @=}}\nEnd.\n" {} {}))))
  (t/testing
      "Indented Standalone Tag / Indented standalone lines should be removed from the template."
    (t/is
     (=
      "Begin.\nEnd.\n"
      (sut/render "Begin.\n  {{=@ @=}}\nEnd.\n" {} {}))))
  (t/testing
      "Standalone Line Endings / \"\\r\\n\" should be considered a newline for standalone tags."
    (t/is (= "|\r\n|" (sut/render "|\r\n{{= @ @ =}}\r\n|" {} {}))))
  (t/testing
      "Standalone Without Previous Line / Standalone tags should not require a newline to precede them."
    (t/is (= "=" (sut/render "  {{=@ @=}}\n=" {} {}))))
  (t/testing
      "Standalone Without Newline / Standalone tags should not require a newline to follow them."
    (t/is (= "=\n" (sut/render "=\n  {{=@ @=}}" {} {}))))
  (t/testing
      "Pair with Padding / Superfluous in-tag whitespace should be ignored."
    (t/is (= "||" (sut/render "|{{= @   @ =}}|" {} {})))))

;; (h/generate-test-cases-from-spec interpolation)

(t/deftest interpolation-test
  (t/testing
      "No Interpolation / Mustache-free templates should render as-is."
    (t/is
     (=
      "Hello from {Mustache}!\n"
      (sut/render "Hello from {Mustache}!\n" {} {}))))
  (t/testing
      "Basic Interpolation / Unadorned tags should interpolate content into the template."
    (t/is
     (=
      "Hello, world!\n"
      (sut/render "Hello, {{subject}}!\n" {:subject "world"} {}))))
  (t/testing
      "HTML Escaping / Basic interpolation should be HTML escaped."
    (t/is
     (=
      "These characters should be HTML escaped: &amp; &quot; &lt; &gt;\n"
      (sut/render
       "These characters should be HTML escaped: {{forbidden}}\n"
       {:forbidden "& \" < >"}
       {}))))
  (t/testing
      "Triple Mustache / Triple mustaches should interpolate without HTML escaping."
    (t/is
     (=
      "These characters should not be HTML escaped: & \" < >\n"
      (sut/render
       "These characters should not be HTML escaped: {{{forbidden}}}\n"
       {:forbidden "& \" < >"}
       {}))))
  (t/testing
      "Ampersand / Ampersand should interpolate without HTML escaping."
    (t/is
     (=
      "These characters should not be HTML escaped: & \" < >\n"
      (sut/render
       "These characters should not be HTML escaped: {{&forbidden}}\n"
       {:forbidden "& \" < >"}
       {}))))
  (t/testing
      "Basic Integer Interpolation / Integers should interpolate seamlessly."
    (t/is
     (=
      "\"85 miles an hour!\""
      (sut/render "\"{{mph}} miles an hour!\"" {:mph 85} {}))))
  (t/testing
      "Triple Mustache Integer Interpolation / Integers should interpolate seamlessly."
    (t/is
     (=
      "\"85 miles an hour!\""
      (sut/render "\"{{{mph}}} miles an hour!\"" {:mph 85} {}))))
  (t/testing
      "Ampersand Integer Interpolation / Integers should interpolate seamlessly."
    (t/is
     (=
      "\"85 miles an hour!\""
      (sut/render "\"{{&mph}} miles an hour!\"" {:mph 85} {}))))
  (t/testing
      "Basic Decimal Interpolation / Decimals should interpolate seamlessly with proper significance."
    (t/is
     (=
      "\"1.21 jiggawatts!\""
      (sut/render "\"{{power}} jiggawatts!\"" {:power 1.21} {}))))
  (t/testing
      "Triple Mustache Decimal Interpolation / Decimals should interpolate seamlessly with proper significance."
    (t/is
     (=
      "\"1.21 jiggawatts!\""
      (sut/render "\"{{{power}}} jiggawatts!\"" {:power 1.21} {}))))
  (t/testing
      "Ampersand Decimal Interpolation / Decimals should interpolate seamlessly with proper significance."
    (t/is
     (=
      "\"1.21 jiggawatts!\""
      (sut/render "\"{{&power}} jiggawatts!\"" {:power 1.21} {}))))
  (t/testing
      "Basic Context Miss Interpolation / Failed context lookups should default to empty strings."
    (t/is
     (=
      "I () be seen!"
      (sut/render "I ({{cannot}}) be seen!" {} {}))))
  (t/testing
      "Triple Mustache Context Miss Interpolation / Failed context lookups should default to empty strings."
    (t/is
     (=
      "I () be seen!"
      (sut/render "I ({{{cannot}}}) be seen!" {} {}))))
  (t/testing
      "Ampersand Context Miss Interpolation / Failed context lookups should default to empty strings."
    (t/is
     (=
      "I () be seen!"
      (sut/render "I ({{&cannot}}) be seen!" {} {}))))
  (t/testing
      "Dotted Names - Basic Interpolation / Dotted names should be considered a form of shorthand for sections."
    (t/is
     (=
      "\"Joe\" == \"Joe\""
      (sut/render
       "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""
       {:person {:name "Joe"}}
       {}))))
  (t/testing
      "Dotted Names - Triple Mustache Interpolation / Dotted names should be considered a form of shorthand for sections."
    (t/is
     (=
      "\"Joe\" == \"Joe\""
      (sut/render
       "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""
       {:person {:name "Joe"}}
       {}))))
  (t/testing
      "Dotted Names - Ampersand Interpolation / Dotted names should be considered a form of shorthand for sections."
    (t/is
     (=
      "\"Joe\" == \"Joe\""
      (sut/render
       "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""
       {:person {:name "Joe"}}
       {}))))
  (t/testing
      "Dotted Names - Arbitrary Depth / Dotted names should be functional to any level of nesting."
    (t/is
     (=
      "\"Phil\" == \"Phil\""
      (sut/render
       "\"{{a.b.c.d.e.name}}\" == \"Phil\""
       {:a {:b {:c {:d {:e {:name "Phil"}}}}}}
       {}))))
  (t/testing
      "Dotted Names - Broken Chains / Any falsey value prior to the last part of the name should yield ''."
    (t/is
     (=
      "\"\" == \"\""
      (sut/render "\"{{a.b.c}}\" == \"\"" {:a {}} {}))))
  (t/testing
      "Dotted Names - Broken Chain Resolution / Each part of a dotted name should resolve only against its parent."
    (t/is
     (=
      "\"\" == \"\""
      (sut/render
       "\"{{a.b.c.name}}\" == \"\""
       {:a {:b {}}, :c {:name "Jim"}}
       {}))))
  (t/testing
      "Dotted Names - Initial Resolution / The first part of a dotted name should resolve as any other name."
    (t/is
     (=
      "\"Phil\" == \"Phil\""
      (sut/render
       "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""
       {:a {:b {:c {:d {:e {:name "Phil"}}}}},
        :b {:c {:d {:e {:name "Wrong"}}}}}
       {}))))
  (t/testing
      "Interpolation - Surrounding Whitespace / Interpolation should not alter surrounding whitespace."
    (t/is
     (= "| --- |" (sut/render "| {{string}} |" {:string "---"} {}))))
  (t/testing
      "Triple Mustache - Surrounding Whitespace / Interpolation should not alter surrounding whitespace."
    (t/is
     (=
      "| --- |"
      (sut/render "| {{{string}}} |" {:string "---"} {}))))
  (t/testing
      "Ampersand - Surrounding Whitespace / Interpolation should not alter surrounding whitespace."
    (t/is
     (= "| --- |" (sut/render "| {{&string}} |" {:string "---"} {}))))
  (t/testing
      "Interpolation - Standalone / Standalone interpolation should not alter surrounding whitespace."
    (t/is
     (= "  ---\n" (sut/render "  {{string}}\n" {:string "---"} {}))))
  (t/testing
      "Triple Mustache - Standalone / Standalone interpolation should not alter surrounding whitespace."
    (t/is
     (=
      "  ---\n"
      (sut/render "  {{{string}}}\n" {:string "---"} {}))))
  (t/testing
      "Ampersand - Standalone / Standalone interpolation should not alter surrounding whitespace."
    (t/is
     (= "  ---\n" (sut/render "  {{&string}}\n" {:string "---"} {}))))
  (t/testing
      "Interpolation With Padding / Superfluous in-tag whitespace should be ignored."
    (t/is
     (= "|---|" (sut/render "|{{ string }}|" {:string "---"} {}))))
  (t/testing
      "Triple Mustache With Padding / Superfluous in-tag whitespace should be ignored."
    (t/is
     (= "|---|" (sut/render "|{{{ string }}}|" {:string "---"} {}))))
  (t/testing
      "Ampersand With Padding / Superfluous in-tag whitespace should be ignored."
    (t/is
     (= "|---|" (sut/render "|{{& string }}|" {:string "---"} {})))))

;; (h/generate-test-cases-from-spec inverted)

(t/deftest inverted-test
  (t/testing
      "Falsey / Falsey sections should have their contents rendered."
    (t/is
     (=
      "\"This should be rendered.\""
      (sut/render
       "\"{{^boolean}}This should be rendered.{{/boolean}}\""
       {:boolean false}
       {}))))
  (t/testing
      "Truthy / Truthy sections should have their contents omitted."
    (t/is
     (=
      "\"\""
      (sut/render
       "\"{{^boolean}}This should not be rendered.{{/boolean}}\""
       {:boolean true}
       {}))))
  (t/testing
      "Context / Objects and hashes should behave like truthy values."
    (t/is
     (=
      "\"\""
      (sut/render
       "\"{{^context}}Hi {{name}}.{{/context}}\""
       {:context {:name "Joe"}}
       {}))))
  (t/testing
      "List / Lists should behave like truthy values."
    (t/is
     (=
      "\"\""
      (sut/render
       "\"{{^list}}{{n}}{{/list}}\""
       {:list [{:n 1} {:n 2} {:n 3}]}
       {}))))
  (t/testing
      "Empty List / Empty lists should behave like falsey values."
    (t/is
     (=
      "\"Yay lists!\""
      (sut/render
       "\"{{^list}}Yay lists!{{/list}}\""
       {:list []}
       {}))))
  (t/testing
      "Doubled / Multiple inverted sections per template should be permitted."
    (t/is
     (=
      "* first\n* second\n* third\n"
      (sut/render
       "{{^bool}}\n* first\n{{/bool}}\n* {{two}}\n{{^bool}}\n* third\n{{/bool}}\n"
       {:two "second", :bool false}
       {}))))
  (t/testing
      "Nested (Falsey) / Nested falsey sections should have their contents rendered."
    (t/is
     (=
      "| A B C D E |"
      (sut/render
       "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
       {:bool false}
       {}))))
  (t/testing
      "Nested (Truthy) / Nested truthy sections should be omitted."
    (t/is
     (=
      "| A  E |"
      (sut/render
       "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
       {:bool true}
       {}))))
  (t/testing
      "Context Misses / Failed context lookups should be considered falsey."
    (t/is
     (=
      "[Cannot find key 'missing'!]"
      (sut/render
       "[{{^missing}}Cannot find key 'missing'!{{/missing}}]"
       {}
       {}))))
  (t/testing
      "Dotted Names - Truthy / Dotted names should be valid for Inverted Section tags."
    (t/is
     (=
      "\"\" == \"\""
      (sut/render
       "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\""
       {:a {:b {:c true}}}
       {}))))
  (t/testing
      "Dotted Names - Falsey / Dotted names should be valid for Inverted Section tags."
    (t/is
     (=
      "\"Not Here\" == \"Not Here\""
      (sut/render
       "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
       {:a {:b {:c false}}}
       {}))))
  (t/testing
      "Dotted Names - Broken Chains / Dotted names that cannot be resolved should be considered falsey."
    (t/is
     (=
      "\"Not Here\" == \"Not Here\""
      (sut/render
       "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
       {:a {}}
       {}))))
  (t/testing
      "Surrounding Whitespace / Inverted sections should not alter surrounding whitespace."
    (t/is
     (=
      " | \t|\t | \n"
      (sut/render
       " | {{^boolean}}\t|\t{{/boolean}} | \n"
       {:boolean false}
       {}))))
  (t/testing
      "Internal Whitespace / Inverted should not alter internal whitespace."
    (t/is
     (=
      " |  \n  | \n"
      (sut/render
       " | {{^boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
       {:boolean false}
       {}))))
  (t/testing
      "Indented Inline Sections / Single-line sections should not alter surrounding whitespace."
    (t/is
     (=
      " NO\n WAY\n"
      (sut/render
       " {{^boolean}}NO{{/boolean}}\n {{^boolean}}WAY{{/boolean}}\n"
       {:boolean false}
       {}))))
  (t/testing
      "Standalone Lines / Standalone lines should be removed from the template."
    (t/is
     (=
      "| This Is\n|\n| A Line\n"
      (sut/render
       "| This Is\n{{^boolean}}\n|\n{{/boolean}}\n| A Line\n"
       {:boolean false}
       {}))))
  (t/testing
      "Standalone Indented Lines / Standalone indented lines should be removed from the template."
    (t/is
     (=
      "| This Is\n|\n| A Line\n"
      (sut/render
       "| This Is\n  {{^boolean}}\n|\n  {{/boolean}}\n| A Line\n"
       {:boolean false}
       {}))))
  (t/testing
      "Standalone Line Endings / \"\\r\\n\" should be considered a newline for standalone tags."
    (t/is
     (=
      "|\r\n|"
      (sut/render
       "|\r\n{{^boolean}}\r\n{{/boolean}}\r\n|"
       {:boolean false}
       {}))))
  (t/testing
      "Standalone Without Previous Line / Standalone tags should not require a newline to precede them."
    (t/is
     (=
      "^\n/"
      (sut/render
       "  {{^boolean}}\n^{{/boolean}}\n/"
       {:boolean false}
       {}))))
  (t/testing
      "Standalone Without Newline / Standalone tags should not require a newline to follow them."
    (t/is
     (=
      "^\n/\n"
      (sut/render
       "^{{^boolean}}\n/\n  {{/boolean}}"
       {:boolean false}
       {}))))
  (t/testing
      "Padding / Superfluous in-tag whitespace should be ignored."
    (t/is
     (=
      "|=|"
      (sut/render
       "|{{^ boolean }}={{/ boolean }}|"
       {:boolean false}
       {})))))

;; (h/generate-test-cases-from-spec partials)

(t/deftest partials-test
  (t/testing
      "Basic Behavior / The greater-than operator should expand to the named partial."
    (t/is
     (=
      "\"from partial\""
      (sut/render
       "\"{{>text}}\""
       {}
       {:partials {:text "from partial"}}))))
  (t/testing
      "Failed Lookup / The empty string should be used when the named partial is not found."
    (t/is (= "\"\"" (sut/render "\"{{>text}}\"" {} {:partials {}}))))
  (t/testing
      "Context / The greater-than operator should operate within the current context."
    (t/is
     (=
      "\"*content*\""
      (sut/render
       "\"{{>partial}}\""
       {:text "content"}
       {:partials {:partial "*{{text}}*"}}))))

  (comment
    "fix in the future"
    (t/testing
        "Recursion / The greater-than operator should properly recurse."
      (t/is
       (=
        "X<Y<>>"
        (sut/render
         "{{>node}}"
         {:content "X", :nodes [{:content "Y", :nodes []}]}
         {:partials
          {:node "{{content}}<{{#nodes}}{{>node}}{{/nodes}}>"}})))))

  (t/testing
      "Surrounding Whitespace / The greater-than operator should not alter surrounding whitespace."
    (t/is
     (=
      "| \t|\t |"
      (sut/render
       "| {{>partial}} |"
       {}
       {:partials {:partial "\t|\t"}}))))
  (t/testing
      "Inline Indentation / Whitespace should be left untouched."
    (t/is
     (=
      "  |  >\n>\n"
      (sut/render
       "  {{data}}  {{> partial}}\n"
       {:data "|"}
       {:partials {:partial ">\n>"}}))))
  (t/testing
      "Standalone Line Endings / \"\\r\\n\" should be considered a newline for standalone tags."
    (t/is
     (=
      "|\r\n>|"
      (sut/render
       "|\r\n{{>partial}}\r\n|"
       {}
       {:partials {:partial ">"}}))))
  (t/testing
      "Standalone Without Previous Line / Standalone tags should not require a newline to precede them."
    (t/is
     (=
      "  >\n  >>"
      (sut/render
       "  {{>partial}}\n>"
       {}
       {:partials {:partial ">\n>"}}))))
  (t/testing
      "Standalone Without Newline / Standalone tags should not require a newline to follow them."
    (t/is
     (=
      ">\n  >\n  >"
      (sut/render
       ">\n  {{>partial}}"
       {}
       {:partials {:partial ">\n>"}}))))
  (t/testing
      "Standalone Indentation / Each line of the partial should be indented before rendering."
    (t/is
     (=
      "\\\n |\n <\n->\n |\n/\n"
      (sut/render
       "\\\n {{>partial}}\n/\n"
       {:content "<\n->"}
       {:partials {:partial "|\n{{{content}}}\n|\n"}}))))
  (t/testing
      "Padding Whitespace / Superfluous in-tag whitespace should be ignored."
    (t/is
     (=
      "|[]|"
      (sut/render
       "|{{> partial }}|"
       {:boolean true}
       {:partials {:partial "[]"}})))))

;; (h/generate-test-cases-from-spec sections)

(t/deftest sections-test
  (t/testing
      "Truthy / Truthy sections should have their contents rendered."
    (t/is
     (=
      "\"This should be rendered.\""
      (sut/render
       "\"{{#boolean}}This should be rendered.{{/boolean}}\""
       {:boolean true}
       {}))))
  (t/testing
      "Falsey / Falsey sections should have their contents omitted."
    (t/is
     (=
      "\"\""
      (sut/render
       "\"{{#boolean}}This should not be rendered.{{/boolean}}\""
       {:boolean false}
       {}))))
  (t/testing
      "Context / Objects and hashes should be pushed onto the context stack."
    (t/is
     (=
      "\"Hi Joe.\""
      (sut/render
       "\"{{#context}}Hi {{name}}.{{/context}}\""
       {:context {:name "Joe"}}
       {}))))
  (t/testing
      "Deeply Nested Contexts / All elements on the context stack should be accessible."
    (t/is
     (=
      "1\n121\n12321\n1234321\n123454321\n1234321\n12321\n121\n1\n"
      (sut/render
       "{{#a}}\n{{one}}\n{{#b}}\n{{one}}{{two}}{{one}}\n{{#c}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{#d}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{#e}}\n{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}\n{{/e}}\n{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}\n{{/d}}\n{{one}}{{two}}{{three}}{{two}}{{one}}\n{{/c}}\n{{one}}{{two}}{{one}}\n{{/b}}\n{{one}}\n{{/a}}\n"
       {:a {:one 1},
        :b {:two 2},
        :c {:three 3},
        :d {:four 4},
        :e {:five 5}}
       {}))))
  (t/testing
      "List / Lists should be iterated; list items should visit the context stack."
    (t/is
     (=
      "\"123\""
      (sut/render
       "\"{{#list}}{{item}}{{/list}}\""
       {:list [{:item 1} {:item 2} {:item 3}]}
       {}))))
  (t/testing
      "Empty List / Empty lists should behave like falsey values."
    (t/is
     (=
      "\"\""
      (sut/render
       "\"{{#list}}Yay lists!{{/list}}\""
       {:list []}
       {}))))
  (t/testing
      "Doubled / Multiple sections per template should be permitted."
    (t/is
     (=
      "* first\n* second\n* third\n"
      (sut/render
       "{{#bool}}\n* first\n{{/bool}}\n* {{two}}\n{{#bool}}\n* third\n{{/bool}}\n"
       {:two "second", :bool true}
       {}))))
  (t/testing
      "Nested (Truthy) / Nested truthy sections should have their contents rendered."
    (t/is
     (=
      "| A B C D E |"
      (sut/render
       "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
       {:bool true}
       {}))))
  (t/testing
      "Nested (Falsey) / Nested falsey sections should be omitted."
    (t/is
     (=
      "| A  E |"
      (sut/render
       "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
       {:bool false}
       {}))))
  (t/testing
      "Context Misses / Failed context lookups should be considered falsey."
    (t/is
     (=
      "[]"
      (sut/render
       "[{{#missing}}Found key 'missing'!{{/missing}}]"
       {}
       {}))))
  (t/testing
      "Implicit Iterator - String / Implicit iterators should directly interpolate strings."
    (t/is
     (=
      "\"(a)(b)(c)(d)(e)\""
      (sut/render
       "\"{{#list}}({{.}}){{/list}}\""
       {:list ["a" "b" "c" "d" "e"]}
       {}))))
  (t/testing
      "Implicit Iterator - Integer / Implicit iterators should cast integers to strings and interpolate."
    (t/is
     (=
      "\"(1)(2)(3)(4)(5)\""
      (sut/render
       "\"{{#list}}({{.}}){{/list}}\""
       {:list [1 2 3 4 5]}
       {}))))
  (t/testing
      "Implicit Iterator - Decimal / Implicit iterators should cast decimals to strings and interpolate."
    (t/is
     (=
      "\"(1.1)(2.2)(3.3)(4.4)(5.5)\""
      (sut/render
       "\"{{#list}}({{.}}){{/list}}\""
       {:list [1.1 2.2 3.3 4.4 5.5]}
       {}))))
  (t/testing
      "Implicit Iterator - Array / Implicit iterators should allow iterating over nested arrays."
    (t/is
     (=
      "\"(123)(abc)\""
      (sut/render
       "\"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}\""
       {:list [[1 2 3] ["a" "b" "c"]]}
       {}))))
  (t/testing
      "Dotted Names - Truthy / Dotted names should be valid for Section tags."
    (t/is
     (=
      "\"Here\" == \"Here\""
      (sut/render
       "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\""
       {:a {:b {:c true}}}
       {}))))
  (t/testing
      "Dotted Names - Falsey / Dotted names should be valid for Section tags."
    (t/is
     (=
      "\"\" == \"\""
      (sut/render
       "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
       {:a {:b {:c false}}}
       {}))))
  (t/testing
      "Dotted Names - Broken Chains / Dotted names that cannot be resolved should be considered falsey."
    (t/is
     (=
      "\"\" == \"\""
      (sut/render
       "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
       {:a {}}
       {}))))
  (t/testing
      "Surrounding Whitespace / Sections should not alter surrounding whitespace."
    (t/is
     (=
      " | \t|\t | \n"
      (sut/render
       " | {{#boolean}}\t|\t{{/boolean}} | \n"
       {:boolean true}
       {}))))
  (t/testing
      "Internal Whitespace / Sections should not alter internal whitespace."
    (t/is
     (=
      " |  \n  | \n"
      (sut/render
       " | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
       {:boolean true}
       {}))))
  (t/testing
      "Indented Inline Sections / Single-line sections should not alter surrounding whitespace."
    (t/is
     (=
      " YES\n GOOD\n"
      (sut/render
       " {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n"
       {:boolean true}
       {}))))
  (t/testing
      "Standalone Lines / Standalone lines should be removed from the template."
    (t/is
     (=
      "| This Is\n|\n| A Line\n"
      (sut/render
       "| This Is\n{{#boolean}}\n|\n{{/boolean}}\n| A Line\n"
       {:boolean true}
       {}))))
  (t/testing
      "Indented Standalone Lines / Indented standalone lines should be removed from the template."
    (t/is
     (=
      "| This Is\n|\n| A Line\n"
      (sut/render
       "| This Is\n  {{#boolean}}\n|\n  {{/boolean}}\n| A Line\n"
       {:boolean true}
       {}))))
  (t/testing
      "Standalone Line Endings / \"\\r\\n\" should be considered a newline for standalone tags."
    (t/is
     (=
      "|\r\n|"
      (sut/render
       "|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|"
       {:boolean true}
       {}))))
  (t/testing
      "Standalone Without Previous Line / Standalone tags should not require a newline to precede them."
    (t/is
     (=
      "#\n/"
      (sut/render
       "  {{#boolean}}\n#{{/boolean}}\n/"
       {:boolean true}
       {}))))
  (t/testing
      "Standalone Without Newline / Standalone tags should not require a newline to follow them."
    (t/is
     (=
      "#\n/\n"
      (sut/render
       "#{{#boolean}}\n/\n  {{/boolean}}"
       {:boolean true}
       {}))))
  (t/testing
      "Padding / Superfluous in-tag whitespace should be ignored."
    (t/is
     (=
      "|=|"
      (sut/render
       "|{{# boolean }}={{/ boolean }}|"
       {:boolean true}
       {})))))

;; (h/generate-test-cases-from-spec %7Elambdas)

(t/deftest lambdas-test
  (t/testing
      "Interpolation / A lambda's return value should be interpolated."
    (t/is
     (=
      "Hello, world!"
      (sut/render
       "Hello, {{lambda}}!"
       {:lambda (fn [] "world")}
       {}))))

  (t/testing
      "Interpolation - Expansion / A lambda's return value should be parsed."
    (t/is
     (=
      "Hello, world!"
      (sut/render
       "Hello, {{lambda}}!"
       {:planet "world",
        :lambda (fn [] "{{planet}}")}
       {}))))

  (t/testing
      "Interpolation - Alternate Delimiters / A lambda's return value should parse with the default delimiters."
    (t/is
     (=
      "Hello, (|planet| => world)!"
      (sut/render
       "{{= | | =}}\nHello, (|&lambda|)!"
       {:planet "world",
        :lambda (fn [] "|planet| => {{planet}}")}
       {}))))

  (t/testing
      "Interpolation - Multiple Calls / Interpolated lambdas should not be cached."
    (t/is
     (=
      "1 == 2 == 3"
      (sut/render
       "{{lambda}} == {{{lambda}}} == {{lambda}}"
       {:lambda (let [g (atom 0)] (fn [] (swap! g inc)))}
       {}))))

  (t/testing
      "Escaping / Lambda results should be appropriately escaped."
    (t/is
     (=
      "<&gt;>"
      (sut/render
       "<{{lambda}}{{{lambda}}}"
       {:lambda (fn [] ">")}
       {}))))

  (t/testing
      "Section / Lambdas used for sections should receive the raw section string."
    (t/is
     (=
      "<yes>"
      (sut/render
       "<{{#lambda}}{{x}}{{/lambda}}>"
       {:x "Error!",
        :lambda (fn [text] (if (= text "{{x}}") "yes" "no"))}
       {}))))

  (t/testing
      "Section - Expansion / Lambdas used for sections should have their results parsed."
    (t/is
     (=
      "<-Earth->"
      (sut/render
       "<{{#lambda}}-{{/lambda}}>"
       {:planet "Earth",
        :lambda (fn [text] (str text "{{planet}}" text))}
       {}))))

  (t/testing
      "Section - Alternate Delimiters / Lambdas used for sections should parse with the current delimiters."
    (t/is
     (=
      "<-{{planet}} => Earth->"
      (sut/render
       "{{= | | =}}<|#lambda|-|/lambda|>"
       {:planet "Earth",
        :lambda (fn [text] (str text "{{planet}} => |planet|" text))}
       {}))))

  (t/testing
      "Section - Multiple Calls / Lambdas used for sections should not be cached."
    (t/is
     (=
      "__FILE__ != __LINE__"
      (sut/render
       "{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}"
       {:lambda (fn [text] (str "__" text "__"))}
       {}))))

  (t/testing
      "Inverted Section / Lambdas used for inverted sections should be considered truthy."
    (t/is
     (=
      "<>"
      (sut/render
       "<{{^lambda}}{{static}}{{/lambda}}>"
       {:static "static",
        :lambda (fn [text] false)}
       {})))))
