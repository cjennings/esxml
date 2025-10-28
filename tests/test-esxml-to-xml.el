;;; test-esxml-to-xml.el --- Unit tests for esxml-to-xml -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:
;; Unit tests for the esxml-to-xml function.
;; This function converts esxml S-expressions to XML strings.
;;
;; Tests cover:
;; - Normal cases: strings, raw-strings, comments, tags with various configurations
;; - Boundary cases: empty values, unicode, deeply nested, special characters
;; - Error cases: invalid inputs that should signal errors

;;; Code:

(require 'ert)
(require 'esxml)

;;; Setup and Teardown

(defun test-esxml-to-xml-setup ()
  "Setup function run before each test."
  ;; No setup needed for these pure functions
  nil)

(defun test-esxml-to-xml-teardown ()
  "Teardown function run after each test."
  ;; No teardown needed
  nil)

;;; Normal Cases

(ert-deftest test-esxml-to-xml-normal-plain-string-returns-string ()
  "Test that a plain string without special characters is returned unchanged."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "Hello World") "Hello World"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-string-with-entities-escapes-them ()
  "Test that XML entities in strings are properly escaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "<hello>") "&lt;hello&gt;"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-string-with-ampersand-escapes-it ()
  "Test that ampersands in strings are properly escaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "foo & bar") "foo &amp; bar"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-string-with-quotes-escapes-them ()
  "Test that quotes in strings are properly escaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "Say \"hello\"") "Say &quot;hello&quot;"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-raw-string-returns-unescaped ()
  "Test that raw-string form returns content without escaping."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(raw-string "<br>")) "<br>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-comment-wraps-in-comment-tags ()
  "Test that comment form wraps content in HTML comment syntax."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(comment nil "This is a comment"))
                 "<!-- This is a comment -->"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-self-closing-tag-no-attrs-returns-tag ()
  "Test that a tag with nil attrs and no body self-closes."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(br nil)) "<br/>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-self-closing-tag-with-attrs-includes-attrs ()
  "Test that self-closing tag with attributes includes them."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(img ((src . "foo.png") (alt . "Foo"))))
                 "<img src=\"foo.png\" alt=\"Foo\"/>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-tag-with-body-no-attrs-returns-tag ()
  "Test that a tag with body but no attributes works correctly."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(p nil "Hello")) "<p>Hello</p>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-tag-with-attrs-and-body-returns-tag ()
  "Test that a tag with both attributes and body works correctly."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(a ((href . "http://example.com")) "Link"))
                 "<a href=\"http://example.com\">Link</a>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-tag-with-multiple-children-concatenates ()
  "Test that a tag with multiple text children concatenates them."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(div nil "Hello" " " "World"))
                 "<div>Hello World</div>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-nested-tags-returns-nested ()
  "Test that nested tags are properly rendered."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(div nil (p nil "Nested")))
                 "<div><p>Nested</p></div>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-mixed-content-renders-correctly ()
  "Test that mixed text and element content renders correctly."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(p nil "Text " (strong nil "bold") " more"))
                 "<p>Text <strong>bold</strong> more</p>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-normal-multiple-attributes-includes-all ()
  "Test that tags with multiple attributes include all of them."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(input ((type . "text") (name . "user") (value . "default"))))
                 "<input type=\"text\" name=\"user\" value=\"default\"/>"))
  (test-esxml-to-xml-teardown))

;;; Boundary Cases

(ert-deftest test-esxml-to-xml-boundary-empty-string-returns-empty ()
  "Test that an empty string returns empty string."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "") ""))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-whitespace-only-string-returns-whitespace ()
  "Test that whitespace-only strings are preserved."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "   ") "   "))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-empty-attrs-list-self-closes ()
  "Test that empty attributes list still allows self-closing."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(br nil)) "<br/>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-empty-string-body-does-not-self-close ()
  "Test that tag with empty string body does not self-close per spec."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(div nil "")) "<div></div>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-empty-attribute-value-includes-attr ()
  "Test that empty attribute values are included."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(input ((disabled . ""))))
                 "<input disabled=\"\"/>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-unicode-chars-preserved ()
  "Test that Unicode characters are preserved in output."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "Hello 世界 🌍") "Hello 世界 🌍"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-attr-value-with-special-chars-escapes ()
  "Test that attribute values with special characters are escaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(a ((title . "Say \"hello\""))))
                 "<a title=\"Say &quot;hello&quot;\"/>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-deeply-nested-renders-correctly ()
  "Test that deeply nested structures render correctly."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(div nil (div nil (div nil (div nil (div nil "Deep"))))))
                 "<div><div><div><div><div>Deep</div></div></div></div></div>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-very-long-string-handles-correctly ()
  "Test that very long strings are handled correctly."
  (test-esxml-to-xml-setup)
  (let ((long-string (make-string 1000 ?a)))
    (should (equal (esxml-to-xml long-string) long-string)))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-raw-string-with-xml-declaration-unescaped ()
  "Test that raw-string preserves XML declarations unescaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(raw-string "<?xml version=\"1.0\"?>"))
                 "<?xml version=\"1.0\"?>"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-comment-with-special-chars-unescaped ()
  "Test that comments preserve special characters unescaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml '(comment nil "Comment with <tags> & stuff"))
                 "<!-- Comment with <tags> & stuff -->"))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-boundary-all-xml-entities-escaped ()
  "Test that all special XML entities are properly escaped."
  (test-esxml-to-xml-setup)
  (should (equal (esxml-to-xml "<>&\"'")
                 "&lt;&gt;&amp;&quot;&apos;"))
  (test-esxml-to-xml-teardown))

;;; Error Cases

(ert-deftest test-esxml-to-xml-error-nil-input-signals-error ()
  "Test that nil input signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml nil))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-invalid-tag-type-signals-error ()
  "Test that non-symbol tag signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '("notasymbol" nil)))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-invalid-attrs-type-signals-error ()
  "Test that non-alist attributes signal an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(div "not-alist")))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-invalid-attr-key-signals-error ()
  "Test that non-symbol attribute keys signal an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(div (("not-symbol" . "value")))))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-invalid-attr-value-signals-error ()
  "Test that non-string attribute values signal an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(div ((key . 123)))))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-malformed-raw-string-missing-arg-signals-error ()
  "Test that raw-string without second element signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(raw-string)))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-malformed-raw-string-wrong-type-signals-error ()
  "Test that raw-string with non-string argument signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(raw-string 123)))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-malformed-comment-signals-error ()
  "Test that malformed comment structure signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(comment "missing-nil")))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-too-short-list-signals-error ()
  "Test that list with length < 2 signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml '(tag)))
  (test-esxml-to-xml-teardown))

(ert-deftest test-esxml-to-xml-error-non-list-non-string-signals-error ()
  "Test that non-list, non-string input signals an error."
  (test-esxml-to-xml-setup)
  (should-error (esxml-to-xml 123))
  (test-esxml-to-xml-teardown))

(provide 'test-esxml-to-xml)
;;; test-esxml-to-xml.el ends here
