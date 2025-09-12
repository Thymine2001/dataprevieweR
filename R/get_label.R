# -*- coding: UTF-8 -*-
get_label <- function(key, lang = "en") {
  labels <- list(
    # App title
    app_title = list(en = "datapreviewR: A data review and QC tool", zh = "\u6570\u636e\u9884\u89c8R\uff1a\u6570\u636e\u5ba1\u67e5\u4e0e\u8d28\u63a7\u5de5\u5177"),
    
    # Basic UI elements
    language = list(en = "Language / \u8bed\u8a00", zh = "\u8bed\u8a00 / Language"),
    upload = list(en = "Upload Data", zh = "\u4e0a\u4f20\u6570\u636e"),
    file_upload = list(en = "Choose File", zh = "\u9009\u62e9\u6587\u4ef6"),
    file_types = list(
      en = "Supported file types: .csv, .tsv, .txt (with headers), .xlsx, .xls, .rds",
      zh = "\u652f\u6301\u6587\u4ef6\u7c7b\u578b\uff1a.csv, .tsv, .txt\uff08\u9700\u5305\u542b\u5217\u540d\uff09\uff0c.xlsx, .xls, .rds"
    ),
    
    # Tab titles
    data_preview = list(en = "Data Preview", zh = "\u6570\u636e\u9884\u89c8"),
    qc_results = list(en = "QC Results", zh = "\u8d28\u63a7\u7ed3\u679c"),
    
    # Column selection
    select_columns = list(en = "Select Column Names for Visualization (Multi-select supported)", zh = "\u9009\u62e9\u7528\u4e8e\u53ef\u89c6\u5316\u7684\u5217\u540d\uff08\u652f\u6301\u591a\u9009\uff09"),
    categorical_vars = list(en = "Select Categorical Variables (e.g., Variety, Farm, etc.)", zh = "\u9009\u62e9\u5206\u7c7b\u53d8\u91cf\uff08\u5982\u54c1\u79cd\u3001\u573a\u7b49\uff09"),
    
    # Plot options
    plot_type = list(en = "Plot Type", zh = "\u56fe\u8868\u7c7b\u578b"),
    histogram = list(en = "Histogram", zh = "\u76f4\u65b9\u56fe"),
    boxplot = list(en = "Boxplot", zh = "\u76d2\u7ebf\u56fe"),
    hist_bin = list(en = "Histogram Bin Size", zh = "\u76f4\u65b9\u56fe Bin \u5927\u5c0f"),
    
    # QC options
    qc_filter_options = list(en = "QC Filter Options", zh = "\u8d28\u63a7\u8fc7\u6ee4\u9009\u9879"),
    qc_mode = list(en = "QC Mode", zh = "\u8d28\u63a7\u6a21\u5f0f"),
    uniform_qc = list(en = "Same Method for All Traits", zh = "\u6240\u6709\u6027\u72b6\u4f7f\u7528\u76f8\u540c\u65b9\u6cd5"),
    individual_qc = list(en = "Different Methods per Trait", zh = "\u6bcf\u4e2a\u6027\u72b6\u4f7f\u7528\u4e0d\u540c\u65b9\u6cd5"),
    configure_qc = list(en = "Configure QC for Each Trait:", zh = "\u4e3a\u6bcf\u4e2a\u6027\u72b6\u914d\u7f6e\u8d28\u63a7\uff1a"),
    
    # Filter types
    filter_type = list(en = "Filter Type", zh = "\u8fc7\u6ee4\u7c7b\u578b"),
    threshold_range = list(en = "Threshold Range", zh = "\u9608\u503c\u8303\u56f4"),
    sd_multiplier = list(en = "Mean +/- Times Standard Deviation Multiplier", zh = "\u5747\u503c +/- \u6807\u51c6\u5dee\u500d\u6570"),
    iqr_multiplier = list(en = "IQR Multiplier", zh = "IQR \u500d\u6570"),
    min_threshold = list(en = "Minimum Threshold", zh = "\u6700\u5c0f\u9608\u503c"),
    max_threshold = list(en = "Maximum Threshold", zh = "\u6700\u5927\u9608\u503c"),
    
    # Action buttons
    apply_filter = list(en = "Apply Filter", zh = "\u5e94\u7528\u8fc7\u6ee4"),
    download_filtered = list(en = "Download Filtered Data", zh = "\u4e0b\u8f7d\u8fc7\u6ee4\u540e\u6570\u636e"),
    download_plot = list(en = "Download Plot (PNG)", zh = "\u4e0b\u8f7d\u56fe\u8868 (PNG)"),
    download_comparison = list(en = "Download Comparison Plot (PNG)", zh = "\u4e0b\u8f7d\u5bf9\u6bd4\u56fe (PNG)"),
    
    # Results titles
    removed_records = list(en = "Summary of Removed Records per Column", zh = "\u6bcf\u5217\u79fb\u9664\u8bb0\u5f55\u6c47\u603b"),
    comparison_means = list(en = "Comparison of Means and Standard Deviations (Pre vs Post QC)", zh = "\u5747\u503c\u548c\u6807\u51c6\u5dee\u5bf9\u6bd4\uff08\u8d28\u63a7\u524d\u540e\uff09"),
    
    # Data summary
    data_summary = list(en = "Data Summary:", zh = "\u6570\u636e\u6458\u8981\uff1a"),
    original_dataset = list(en = "Original dataset:", zh = "\u539f\u59cb\u6570\u636e\u96c6\uff1a"),
    after_filtering = list(en = "After categorical filtering:", zh = "\u5206\u7c7b\u8fc7\u6ee4\u540e\uff1a"),
    filtered_out = list(en = "Filtered out:", zh = "\u5df2\u8fc7\u6ee4\uff1a"),
    rows = list(en = "rows", zh = "\u884c"),
    
    # Filter information
    filter_by = list(en = "Filter by:", zh = "\u6309\u4ee5\u4e0b\u6761\u4ef6\u8fc7\u6ee4\uff1a"),
    no_categorical = list(en = "No categorical variables selected.", zh = "\u672a\u9009\u62e9\u5206\u7c7b\u53d8\u91cf\u3002"),
    
    # Error messages
    no_data_plot = list(en = "No numeric data available for plotting.", zh = "\u6ca1\u6709\u53ef\u7528\u4e8e\u7ed8\u56fe\u7684\u6570\u636e\u3002"),
    no_data_comparison = list(en = "No data available for comparison plots.", zh = "\u6ca1\u6709\u53ef\u7528\u4e8e\u5bf9\u6bd4\u56fe\u7684\u6570\u636e\u3002"),
    no_data_download = list(en = "No data available to download.", zh = "\u6ca1\u6709\u53ef\u4e0b\u8f7d\u7684\u6570\u636e\u3002"),
    file_error = list(en = "Error reading file. Please check the file format.", zh = "\u8bfb\u53d6\u6587\u4ef6\u9519\u8bef\u3002\u8bf7\u68c0\u67e5\u6587\u4ef6\u683c\u5f0f\u3002"),
    unsupported_file = list(en = "Unsupported file type:", zh = "\u4e0d\u652f\u6301\u7684\u6587\u4ef6\u7c7b\u578b\uff1a")
  )
  labels[[key]][[lang]] %||% labels[[key]][["en"]]
}