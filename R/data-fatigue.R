## load data, data from Giloteaux et al (2016)
# otu_table = read.delim(file = "data-raw/feature_table.tsv", skip = 1,
#                        comment.char = "", stringsAsFactors = F)
# rownames(otu_table) = str_c("OTU", str_pad(rownames(otu_table), width=3, pad="0"))
# refseq = DNAStringSet(otu_table$X.OTU.ID)
# names(refseq) = rownames(otu_table)
# otu_table = otu_table[,-1]
#
# tax_table = read.delim(file = "data-raw/taxonomy.tsv", stringsAsFactors = F)
# tax_table = column_to_rownames(tax_table, "Feature.ID")
# tax_table = tax_table[as.character(refseq),]
# tax_table = str_split(tax_table, ";", n=7, simplify = T)
# tax_table[grepl("^[kpcofgs]{1}__$",tax_table)] = NA
# tax_table[tax_table == ""] = NA
# tax_table = as.data.frame(tax_table)
# rownames(tax_table) = rownames(otu_table)
# colnames(tax_table) = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
#
# sam_data = read.delim("data-raw/sample-metadata.tsv", comment.char = "", stringsAsFactors = F) %>% column_to_rownames("X.SampleID")
# sam_data = sam_data[colnames(otu_table),]
#
# otu_table = otu_table(otu_table, taxa_are_rows = T)
# tax_table = tax_table(as.matrix(tax_table))
#
# sam_data = sample_data(sam_data)
#
# fatigue = phyloseq(otu_table, tax_table, sam_data, refseq)
# fatigue = fix_duplicate_tax(fatigue)
################################################################################
#' @title (Data) Chronic Fatigue Syndrome dataset
#' @description
#' This dataset contains 87 individuals with 48 diseased patients with the
#' chronic fatigue syndrome, and 39 healthy controls. The data were sequenced on
#' an Illumina MiSeq using the Earth Microbiome Project hypervariable region 4
#' (V4) 16S rRNA sequencing protocol. The data processing procedure can be found
#' on the qiime2 documentation website.
#' @references Giloteaux, L., Goodrich, J. K., Walters, W. A., Levine, S. M.,
#' Ley, R. E., & Hanson, M. R. (2016).
#' Reduced diversity and altered composition of the gut microbiome in
#' individuals with myalgic encephalomyelitis/chronic fatigue syndrome.
#' Microbiome, 4(1), 30.
#' @name data-fatigue
#' @aliases fatigue
#' @docType data
#' @author Guiloteaux et al.
#' @keywords data
NA
