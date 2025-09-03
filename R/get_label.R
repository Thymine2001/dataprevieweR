# -*- coding: UTF-8 -*-
get_label <- function(key, lang = "en") {
  labels <- list(
    language = list(en = "Language / \u8bed\u8a00", zh = "\u8bed\u8a00 / Language"),
    upload = list(en = "Upload Data", zh = "\u4e0a\u4f20\u6570\u636e"),
    file_upload = list(en = "Choose File", zh = "\u9009\u62e9\u6587\u4ef6"),
    supported_types = list(
      en = "Supported file types: .csv, .tsv, .txt (with headers), .xlsx, .xls, .rds",
      zh = "\u652f\u6301\u6587\u4ef6\u7c7b\u578b\uff1a.csv, .tsv, .txt\uff08\u9700\u5305\u542b\u5217\u540d\uff09\uff0c.xlsx, .xls, .rds"
    ),
    preview = list(en = "Data Preview", zh = "\u6570\u636e\u9884\u89c8"),
    qc = list(en = "QC Results", zh = "\u8d28\u63a7\u7ed3\u679c"),
    select_columns = list(en = "Select Columns", zh = "\u9009\u62e9\u5217"),
    plot_type = list(en = "Plot Type", zh = "\u56fe\u7c7b\u578b"),
    hist_bin = list(en = "Histogram Bin Size", zh = "\u76f4\u65b9\u56fe Bin \u5927\u5c0f"),
    filter_options = list(en = "QC Filter Options", zh = "\u8d28\u63a7\u9009\u9879"),
    filter_type = list(en = "Filter Type", zh = "\u8fc7\u6ee4\u7c7b\u578b"),
    threshold_range = list(en = "Threshold Range", zh = "\u9608\u503c\u8303\u56f4"),
    sd_multiplier = list(en = "Mean +/- Times Standard Deviation Multiplier", zh = "\u5747\u503c\u00b1\u6807\u51c6\u5dee\u500d\u6570"),
    iqr_multiplier = list(en = "IQR Multiplier", zh = "IQR \u500d\u6570"),
    min_threshold = list(en = "Minimum Threshold", zh = "\u6700\u5c0f\u9608\u503c"),
    max_threshold = list(en = "Maximum Threshold", zh = "\u6700\u5927\u9608\u503c"),
    apply_filter = list(en = "Apply Filter", zh = "\u5e94\u7528\u8fc7\u6ee4"),
    download_filtered = list(en = "Download Filtered Data", zh = "\u4e0b\u8f7d\u8fc7\u6ee4\u540e\u6570\u636e")
  )
  labels[[key]][[lang]] %||% labels[[key]][["en"]]
}
