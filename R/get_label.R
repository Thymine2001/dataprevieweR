get_label <- function(key, lang = "en") {
  labels <- list(
    language = list(en = "Language / 语言", zh = "语言 / Language"),
    upload = list(en = "Upload Data", zh = "上传数据"),
    file_upload = list(en = "Choose File", zh = "选择文件"),
    supported_types = list(
      en = "Supported file types: .csv, .tsv, .txt (with headers), .xlsx, .xls, .rds",
      zh = "支持文件类型：.csv, .tsv, .txt（需包含列名），.xlsx, .xls, .rds"
    ),
    preview = list(en = "Data Preview", zh = "数据预览"),
    qc = list(en = "QC Results", zh = "质控结果"),
    select_columns = list(en = "Select Columns", zh = "选择列"),
    plot_type = list(en = "Plot Type", zh = "图类型"),
    hist_bin = list(en = "Histogram Bin Size", zh = "直方图 Bin 大小"),
    filter_options = list(en = "QC Filter Options", zh = "质控选项"),
    filter_type = list(en = "Filter Type", zh = "过滤类型"),
    threshold_range = list(en = "Threshold Range", zh = "阈值范围"),
    sd_multiplier = list(en = "Mean +/- Times Standard Deviation Multiplier", zh = "均值±标准差倍数"),
    iqr_multiplier = list(en = "IQR Multiplier", zh = "IQR 倍数"),
    min_threshold = list(en = "Minimum Threshold", zh = "最小阈值"),
    max_threshold = list(en = "Maximum Threshold", zh = "最大阈值"),
    apply_filter = list(en = "Apply Filter", zh = "应用过滤"),
    download_filtered = list(en = "Download Filtered Data", zh = "下载过滤后数据")
  )
  labels[[key]][[lang]] %||% labels[[key]][["en"]]
}
