# -*- coding: UTF-8 -*-
get_label <- function(key, lang = "en") {
  labels <- list(
    # App title
    app_title = list(en = "datapreviewR: A data review and QC tool", zh = "数据预览R：数据审查与质控工具"),
    
    # Basic UI elements
    language = list(en = "Language / 语言", zh = "语言 / Language"),
    upload = list(en = "Upload Data", zh = "上传数据"),
    file_upload = list(en = "Choose File", zh = "选择文件"),
    supported_types = list(
      en = "Supported file types: .csv, .tsv, .txt (with headers), .xlsx, .xls, .rds",
      zh = "支持文件类型：.csv, .tsv, .txt（需包含列名），.xlsx, .xls, .rds"
    ),
    
    # Tab titles
    data_preview = list(en = "Data Preview", zh = "数据预览"),
    qc_results = list(en = "QC Results", zh = "质控结果"),
    
    # Column selection
    select_columns = list(en = "Select Column Names for Visualization (Multi-select supported)", zh = "选择用于可视化的列名（支持多选）"),
    categorical_vars = list(en = "Select Categorical Variables (e.g., Variety, Farm, etc.)", zh = "选择分类变量（如品种、场等）"),
    
    # Plot options
    plot_type = list(en = "Plot Type", zh = "图表类型"),
    histogram = list(en = "Histogram", zh = "直方图"),
    boxplot = list(en = "Boxplot", zh = "箱线图"),
    hist_bin = list(en = "Histogram Bin Size", zh = "直方图 Bin 大小"),
    
    # QC options
    qc_filter_options = list(en = "QC Filter Options", zh = "质控过滤选项"),
    qc_mode = list(en = "QC Mode", zh = "质控模式"),
    uniform_qc = list(en = "Same Method for All Traits", zh = "所有性状使用相同方法"),
    individual_qc = list(en = "Different Methods per Trait", zh = "每个性状使用不同方法"),
    configure_qc = list(en = "Configure QC for Each Trait:", zh = "为每个性状配置质控："),
    
    # Filter types
    filter_type = list(en = "Filter Type", zh = "过滤类型"),
    threshold_range = list(en = "Threshold Range", zh = "阈值范围"),
    sd_multiplier = list(en = "Mean +/- Times Standard Deviation Multiplier", zh = "均值 ± 标准差倍数"),
    iqr_multiplier = list(en = "IQR Multiplier", zh = "IQR 倍数"),
    min_threshold = list(en = "Minimum Threshold", zh = "最小阈值"),
    max_threshold = list(en = "Maximum Threshold", zh = "最大阈值"),
    
    # Actions
    apply_filter = list(en = "Apply Filter", zh = "应用过滤"),
    download_filtered = list(en = "Download Filtered Data", zh = "下载过滤后数据"),
    download_plot = list(en = "Download Plot (PNG)", zh = "下载图表 (PNG)"),
    download_comparison = list(en = "Download Comparison Plot (PNG)", zh = "下载对比图 (PNG)"),
    
    # QC Results
    removed_records = list(en = "Summary of Removed Records per Column", zh = "每列移除记录汇总"),
    comparison_means = list(en = "Comparison of Means and Standard Deviations (Pre vs Post QC)", zh = "均值和标准差对比（质控前后）"),
    
    # Data summary
    data_summary = list(en = "Data Summary:", zh = "数据摘要："),
    original_dataset = list(en = "Original dataset:", zh = "原始数据集："),
    after_filtering = list(en = "After categorical filtering:", zh = "分类过滤后："),
    filtered_out = list(en = "Filtered out:", zh = "已过滤："),
    rows = list(en = "rows", zh = "行"),
    
    # Categorical filters
    filter_by = list(en = "Filter by:", zh = "按以下条件过滤："),
    no_categorical = list(en = "No categorical variables selected.", zh = "未选择分类变量。"),
    
    # Messages
    no_data_plot = list(en = "No numeric data available for plotting.", zh = "没有可用于绘图的数据。"),
    no_data_comparison = list(en = "No data available for comparison plots.", zh = "没有可用于对比图的数据。"),
    no_data_download = list(en = "No data available to download.", zh = "没有可下载的数据。"),
    file_error = list(en = "Error reading file. Please check the file format.", zh = "读取文件错误。请检查文件格式。"),
    unsupported_file = list(en = "Unsupported file type:", zh = "不支持的文件类型：")
  )
  labels[[key]][[lang]] %||% labels[[key]][["en"]]
}
