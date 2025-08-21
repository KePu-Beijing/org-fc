(require 'ert)
(require 'org-fc-core)

(ert-deftest org-fc-language-guess-japanese ()
  (should (equal "ja" (org-fc--guess-language-code "かなカナ"))))

(ert-deftest org-fc-language-guess-english ()
  (should (equal "en" (org-fc--guess-language-code "hello"))))

(ert-deftest org-fc-language-guess-ambiguous ()
  (should-not (org-fc--guess-language-code "漢字")))
