TAILORINGS=$(patsubst %,data/tailorings/%.txt, af ar as az be bn ca cs cu cy da de_at_ph de_phone dsb ee eo es es_trad et fa fi fi_phone fil fo fr_ca gu ha haw he hi hr hu hy ig is ja kk kl kn ko kok lkt ln lt lv mk ml mr mt nb nn nso om or pa pl ro sa se si si_dict sk sl sq sr sv sv_refo ta te th tn to tr ug_cyrl uk ur vi vo wae wo yo zh zh_big5 zh_gb zh_pin zh_strk zh_zhu)
CJK=$(patsubst %,data/cjk/%.txt, Big5 GB2312 JISX0208 Pinyin Stroke Zhuyin)

test-cabal:
	cabal build --enable-tests -fexecutable
	cabal test unit --test-option=--hide-successes
	cabal run doctests -fdoctests

test-stack:
	stack test \
	  --flag unicode-collation:executable && \
	  stack runghc test/doctests.hs

bench:
	stack bench --benchmark-arguments="+RTS -T -RTS"

ghci:
	stack ghci --ghc-options=-XOverloadedStrings unicode-collation:lib unicode-collation:test:unit
clean:
	stack clean

tailorings: $(TAILORINGS) $(CJK)

data/tailorings/%.txt: Unicode-Collate-1.29/Collate/Locale/%.pl
	awk '/^ENTRY/{exit} f; /ENTRY/{f=1}' $< > $@

data/cjk/%.txt: Unicode-Collate-1.29/Collate/CJK/%.pm
	awk '/^__END__/{exit} f; /^__DATA__/{f=1}' $< > $@

Unicode-Collate-1.29/Collate/Locale/%.pl: Unicode-Collate-1.29.tar.gz
	tar xvzf $< $@

Unicode-Collate-1.29.tar.gz:
	wget https://cpan.metacpan.org/authors/id/S/SA/SADAHIRO/Unicode-Collate-1.29.tar.gz

lint:
	hlint src app test benchmark

.PHONY: test bench ghci clean tailorings lint
