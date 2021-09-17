(require 'tools)

(defmacro with-test-buffer (text &rest body)
  `(with-temp-buffer
     (insert ,text)
     (move-beginning-of-line 1)
     ,@body))

(describe
 "A camelcase suite"
 (it "basic"
     (with-test-buffer "foo_bar_maurice"
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "FooBarMaurice")
       (expect (point) :to-equal 14)))
 (it "two words"
     (with-test-buffer "foo_bar maurice"
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "FooBar maurice")
       (expect (point) :to-equal 7)
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "FooBar Maurice")
       (expect (point) :to-equal 15)))
 (it "basic: special"
     (with-test-buffer "__foo__bar"
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "__FooBar")))
 (it "dashed"
     (with-test-buffer "foo-bar-maurice"
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "FooBarMaurice")))
 (it "leading"
     (with-test-buffer "__foo-bar"
       (camelcase)
       (expect (thing-at-point 'line t) :to-equal "__FooBar"))))

(describe
 "A underscore suite"
 (it "basic"
     (with-test-buffer "FooBar"
       (underscore)
       (expect (thing-at-point 'line t) :to-equal "foo_bar")))
 (it "upper part"
     (with-test-buffer "FooBAR"
       (underscore)
       (expect (thing-at-point 'line t) :to-equal "foo_bar"))))

 (provide 'test-tools)
