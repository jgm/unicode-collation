{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Collate.Tailorings
 ( ducetCollation
 , tailorings )
where

import Text.Collate.Lang
import Text.Collate.TH
import Text.Collate.Collation (suppressContractions, insertElements,
                               Collation, CollationElement(..))
import Data.Binary (decode)

-- | The DUCET collation defined in allkeys.txt.
ducetCollation :: Collation
ducetCollation = decode $! $(genCollation "data/allkeys.txt")

applyCJKOverrides :: [Int] -> Collation -> Collation
applyCJKOverrides cps coll = foldr addElt coll (zip cps [0x8000..])
 where
  addElt (cp, weight) = insertElements [cp]
                         [CollationElement False weight 0x0020 0x0002 0x0000]

-- | An association list matching 'Lang's with tailored 'Collation's.
tailorings :: [(Lang, Collation)]
tailorings =
  [("af", decode $(genCollation "data/tailorings/af.txt"))
  ,("ar", decode $(genCollation "data/tailorings/ar.txt"))
  ,("as", decode $(genCollation "data/tailorings/as.txt"))
  ,("az", decode $(genCollation "data/tailorings/az.txt"))
  ,("be", decode $(genCollation "data/tailorings/be.txt"))
  ,("bn", decode $(genCollation "data/tailorings/bn.txt"))
  ,("ca", decode $(genCollation "data/tailorings/ca.txt"))
  ,("cs", decode $(genCollation "data/tailorings/cs.txt"))
  ,("cu", suppressContractions [0x0418, 0x0438] $
            decode $(genCollation "data/tailorings/cu.txt"))
  ,("cy", decode $(genCollation "data/tailorings/cy.txt"))
  ,("da", decode $(genCollation "data/tailorings/da.txt"))
  ,("de_AT_u_co_phonebk", decode $(genCollation "data/tailorings/de_at_ph.txt"))
  ,("de_u_co_phonebk", decode $(genCollation "data/tailorings/de_phone.txt"))
  ,("dsb", decode $(genCollation "data/tailorings/dsb.txt"))
  ,("ee", decode $(genCollation "data/tailorings/ee.txt"))
  ,("eo", decode $(genCollation "data/tailorings/eo.txt"))
  ,("es", decode $(genCollation "data/tailorings/es.txt"))
  ,("es_u_co_trad", decode $(genCollation "data/tailorings/es_trad.txt"))
  ,("et", decode $(genCollation "data/tailorings/et.txt"))
  ,("fa", decode $(genCollation "data/tailorings/fa.txt"))
  ,("fi", decode $(genCollation "data/tailorings/fi.txt"))
  ,("fi_u_co_phonebk", decode $(genCollation "data/tailorings/fi_phone.txt"))
  ,("fil", decode $(genCollation "data/tailorings/fil.txt"))
  ,("fo", decode $(genCollation "data/tailorings/fo.txt"))
  ,("fr_CA", decode $(genCollation "data/tailorings/fr_ca.txt"))
  ,("gu", decode $(genCollation "data/tailorings/gu.txt"))
  ,("ha", decode $(genCollation "data/tailorings/ha.txt"))
  ,("haw", decode $(genCollation "data/tailorings/haw.txt"))
  ,("he", decode $(genCollation "data/tailorings/he.txt"))
  ,("hi", decode $(genCollation "data/tailorings/hi.txt"))
  ,("hr", decode $(genCollation "data/tailorings/hr.txt"))
  ,("hu", decode $(genCollation "data/tailorings/hu.txt"))
  ,("hy", decode $(genCollation "data/tailorings/hy.txt"))
  ,("ig", decode $(genCollation "data/tailorings/ig.txt"))
  ,("is", decode $(genCollation "data/tailorings/is.txt"))
  ,("ja", decode $(genCollation "data/tailorings/ja.txt"))
  ,("kk", decode $(genCollation "data/tailorings/kk.txt"))
  ,("kl", decode $(genCollation "data/tailorings/kl.txt"))
  ,("kn", decode $(genCollation "data/tailorings/kn.txt"))
  ,("ko", decode $(genCollation "data/tailorings/ko.txt"))
  ,("kok", decode $(genCollation "data/tailorings/kok.txt"))
  ,("lkt", decode $(genCollation "data/tailorings/lkt.txt"))
  ,("ln", decode $(genCollation "data/tailorings/ln.txt"))
  ,("lt", decode $(genCollation "data/tailorings/lt.txt"))
  ,("lv", decode $(genCollation "data/tailorings/lv.txt"))
  ,("mk", suppressContractions [0x0418, 0x0438] $
           decode $(genCollation "data/tailorings/mk.txt"))
  ,("ml", decode $(genCollation "data/tailorings/ml.txt"))
  ,("mr", decode $(genCollation "data/tailorings/mr.txt"))
  ,("mt", decode $(genCollation "data/tailorings/mt.txt"))
  ,("nb", decode $(genCollation "data/tailorings/nb.txt"))
  ,("nn", decode $(genCollation "data/tailorings/nn.txt"))
  ,("nso", decode $(genCollation "data/tailorings/nso.txt"))
  ,("om", decode $(genCollation "data/tailorings/om.txt"))
  ,("or", decode $(genCollation "data/tailorings/or.txt"))
  ,("pa", decode $(genCollation "data/tailorings/pa.txt"))
  ,("pl", decode $(genCollation "data/tailorings/pl.txt"))
  ,("ro", decode $(genCollation "data/tailorings/ro.txt"))
  ,("sa", decode $(genCollation "data/tailorings/sa.txt"))
  ,("se", decode $(genCollation "data/tailorings/se.txt"))
  ,("si", decode $(genCollation "data/tailorings/si.txt"))
  ,("si_u_co_dict", decode $(genCollation "data/tailorings/si_dict.txt"))
  ,("sk", decode $(genCollation "data/tailorings/sk.txt"))
  ,("sl", decode $(genCollation "data/tailorings/sl.txt"))
  ,("sq", decode $(genCollation "data/tailorings/sq.txt"))
  ,("sr", suppressContractions [0x0418, 0x0438] $
            decode $(genCollation "data/tailorings/sr.txt"))
  ,("sv", decode $(genCollation "data/tailorings/sv.txt"))
  ,("sv_u_co_reformed", decode $(genCollation "data/tailorings/sv_refo.txt"))
  ,("ta", decode $(genCollation "data/tailorings/ta.txt"))
  ,("te", decode $(genCollation "data/tailorings/te.txt"))
  ,("th", decode $(genCollation "data/tailorings/th.txt"))
  ,("tn", decode $(genCollation "data/tailorings/tn.txt"))
  ,("to", decode $(genCollation "data/tailorings/to.txt"))
  ,("tr", decode $(genCollation "data/tailorings/tr.txt"))
  ,("ug_Cyrl", decode $(genCollation "data/tailorings/ug_cyrl.txt"))
  ,("uk", decode $(genCollation "data/tailorings/uk.txt"))
  ,("ur", decode $(genCollation "data/tailorings/ur.txt"))
  ,("vi", decode $(genCollation "data/tailorings/vi.txt"))
  ,("vo", decode $(genCollation "data/tailorings/vo.txt"))
  ,("wae", decode $(genCollation "data/tailorings/wae.txt"))
  ,("wo", decode $(genCollation "data/tailorings/wo.txt"))
  ,("yo", decode $(genCollation "data/tailorings/yo.txt"))
  ,("zh", decode $(genCollation "data/tailorings/zh.txt"))
  ,("zh_u_co_big5han", applyCJKOverrides
                    (decode $(genCJKOverrides "data/cjk/Big5.txt")) $
                    decode $(genCollation "data/tailorings/zh_big5.txt"))
  ,("zh_u_co_gb2312", applyCJKOverrides
                      (decode $(genCJKOverrides "data/cjk/GB2312.txt")) $
                      decode $(genCollation "data/tailorings/zh_gb.txt"))
  ,("zh_u_co_pinyin", applyCJKOverrides
                      (decode $(genCJKOverrides "data/cjk/Pinyin.txt")) $
                       decode $(genCollation "data/tailorings/zh_pin.txt"))
  ,("zh_u_co_stroke", applyCJKOverrides
                        (decode $(genCJKOverrides "data/cjk/Stroke.txt")) $
                        decode $(genCollation "data/tailorings/zh_strk.txt"))
  ,("zh_u_co_zhuyin", applyCJKOverrides
                       (decode $(genCJKOverrides "data/cjk/Zhuyin.txt")) $
                      decode $(genCollation "data/tailorings/zh_zhu.txt"))
  ]
