# load("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data/microbiome_and_metabolome.Rdata")
#
# physeq = phyloseq(
#     otu_table(microbiome$edata, taxa_are_rows = T),
#     tax_table(as.matrix(microbiome$fdata)),
#     sample_data(microbiome$pdata)
# )
# physeq = fix_duplicate_tax(physeq)
# tr = parsePhyloseq(physeq, node.size.scale = 1.25)
# p = tree.backbone(tr, size=1)
#
# spy = summarizeFromPhyloseq(physeq)
# design = model.matrix(data = as(sample_data(physeq), "data.frame"),
#                       ~Subject + 1)
# spys_de = spy_to_deseq2(spy, design, resultsName = "SubjectPatient")
#
# # limma
# physeq_prop = transform_sample_counts(physeq, function(x) x/sum(x))
#
# spy = summarizeFromPhyloseq(physeq_prop)
# spys_lm = spy_to_limma(spy, design, transform = "log", p.value = 13, coef=13)
#
#
# anno.data = create_annodata(spys_de, coef = "pvalue", cutoff = 0.05)
#
# clade.anno(p, anno.data, anno.depth = 3)
#
