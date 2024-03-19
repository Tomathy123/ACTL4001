mapping_codes <- c('', 'A00-B99', 'C00-D48', 'D50-D89', 'E00-E88', 'F01-F99', 'G00-G98', 'I00-I99', 'J00-J98', 'K00-K92', 'L00-L98', 
             'M00-M99', 'N00-N98','O00-O99', 'Q00-Q99', 'R00-R99', 'V01-Y89')

mapping_categories <- c(
  'Unknown',
  'Certain infectious and parasitic diseases',
  'Neoplasms',
  'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism',
  'Endocrine, nutritional and metabolic diseases',
  'Mental and behavioural disorders',
  'Diseases of the nervous system',
  'Diseases of the circulatory system',
  'Diseases of the respiratory system',
  'Diseases of the digestive system',
  'Diseases of the skin and subcutaneous tissue',
  'Diseases of the musculoskeletal system and connective tissue',
  'Diseases of the genitourinary system',
  'Pregnancy, childbirth and the puerperium',
  'Congenital malformations, deformations and chromosomal abnormalities',
  'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified',
  'External causes of morbidity and mortality'
)

mapping <- data.frame(mapping_codes, mapping_categories)
