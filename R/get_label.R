# -*- coding: UTF-8 -*-
get_label <- function(key, lang = "en") {
  labels <- list(
    # App title
    app_title = list(en = "datapreviewR: A data review and QC tool", zh = "\u6570\u636e\u9884\u89c8R\uff1a\u6570\u636e\u5ba1\u67e5\u4e0e\u8d28\u63a7\u5de5\u5177", pt = "datapreviewR: Uma ferramenta de revis\u00e3o e controle de qualidade de dados"),
    
    # Basic UI elements
    language = list(en = "Language / \u8bed\u8a00 / Idioma", zh = "\u8bed\u8a00 / Language / Idioma", pt = "Idioma / Language / \u8bed\u8a00"),
    upload = list(en = "Upload Data", zh = "\u4e0a\u4f20\u6570\u636e", pt = "Carregar Dados"),
    file_upload = list(en = "Choose File", zh = "\u9009\u62e9\u6587\u4ef6", pt = "Escolher Arquivo"),
    file_types = list(
      en = "Supported file types: .csv, .tsv, .txt (with headers), .xlsx, .xls, .rds",
      zh = "\u652f\u6301\u6587\u4ef6\u7c7b\u578b\uff1a.csv, .tsv, .txt\uff08\u9700\u5305\u542b\u5217\u540d\uff09\uff0c.xlsx, .xls, .rds",
      pt = "Tipos de arquivo suportados: .csv, .tsv, .txt (com cabe\u00e7alhos), .xlsx, .xls, .rds"
    ),
    
    # Tab titles
    data_preview = list(en = "Data Preview", zh = "\u6570\u636e\u9884\u89c8", pt = "Pr\u00e9-visualiza\u00e7\u00e3o dos Dados"),
    qc_results = list(en = "QC Results", zh = "\u8d28\u63a7\u7ed3\u679c", pt = "Resultados de QC"),
    
    # Column selection
    select_columns = list(en = "Select Column Names for Visualization (Multi-select supported)", zh = "\u9009\u62e9\u7528\u4e8e\u53ef\u89c6\u5316\u7684\u5217\u540d\uff08\u652f\u6301\u591a\u9009\uff09", pt = "Selecionar Nomes de Colunas para Visualiza\u00e7\u00e3o (Suporte a m\u00faltipla sele\u00e7\u00e3o)"),
    categorical_vars = list(en = "Select Categorical Variables (e.g., Variety, Farm, etc.)", zh = "\u9009\u62e9\u5206\u7c7b\u53d8\u91cf\uff08\u5982\u54c1\u79cd\u3001\u573a\u7b49\uff09", pt = "Selecionar Vari\u00e1veis Categ\u00f3ricas (ex: Variedade, Fazenda, etc.)"),
    
    # Plot options
    plot_type = list(en = "Plot Type", zh = "\u56fe\u8868\u7c7b\u578b", pt = "Tipo de Gr\u00e1fico"),
    histogram = list(en = "Histogram", zh = "\u76f4\u65b9\u56fe", pt = "Histograma"),
    boxplot = list(en = "Boxplot", zh = "\u76d2\u7ebf\u56fe", pt = "Boxplot"),
    hist_bin = list(en = "Histogram Bin Size", zh = "\u76f4\u65b9\u56fe Bin \u5927\u5c0f", pt = "Tamanho do Bin do Histograma"),
    
    # QC options
    qc_filter_options = list(en = "QC Filter Options", zh = "\u8d28\u63a7\u8fc7\u6ee4\u9009\u9879", pt = "Op\u00e7\u00f5es de Filtro QC"),
    qc_mode = list(en = "QC Mode", zh = "\u8d28\u63a7\u6a21\u5f0f", pt = "Modo QC"),
    uniform_qc = list(en = "Same Method for All Traits", zh = "\u6240\u6709\u6027\u72b6\u4f7f\u7528\u76f8\u540c\u65b9\u6cd5", pt = "Mesmo M\u00e9todo para Todas as Caracter\u00edsticas"),
    individual_qc = list(en = "Different Methods per Trait", zh = "\u6bcf\u4e2a\u6027\u72b6\u4f7f\u7528\u4e0d\u540c\u65b9\u6cd5", pt = "M\u00e9todos Diferentes por Caracter\u00edstica"),
    configure_qc = list(en = "Configure QC for Each Trait:", zh = "\u4e3a\u6bcf\u4e2a\u6027\u72b6\u914d\u7f6e\u8d28\u63a7\uff1a", pt = "Configurar QC para Cada Caracter\u00edstica:"),
    
    # Filter types
    filter_type = list(en = "Filter Type", zh = "\u8fc7\u6ee4\u7c7b\u578b", pt = "Tipo de Filtro"),
    threshold_range = list(en = "Threshold Range", zh = "\u9608\u503c\u8303\u56f4", pt = "Faixa de Limiar"),
    sd_multiplier = list(en = "Mean +/- Times Standard Deviation Multiplier", zh = "\u5747\u503c +/- \u6807\u51c6\u5dee\u500d\u6570", pt = "M\u00e9dia +/- Vezes Multiplicador do Desvio Padr\u00e3o"),
    iqr_multiplier = list(en = "IQR Multiplier", zh = "IQR \u500d\u6570", pt = "Multiplicador IQR"),
    min_threshold = list(en = "Minimum Threshold", zh = "\u6700\u5c0f\u9608\u503c", pt = "Limiar M\u00ednimo"),
    max_threshold = list(en = "Maximum Threshold", zh = "\u6700\u5927\u9608\u503c", pt = "Limiar M\u00e1ximo"),
    
    # Action buttons
    apply_filter = list(en = "Apply Filter", zh = "\u5e94\u7528\u8fc7\u6ee4", pt = "Aplicar Filtro"),
    download_filtered = list(en = "Download Filtered Data", zh = "\u4e0b\u8f7d\u8fc7\u6ee4\u540e\u6570\u636e", pt = "Baixar Dados Filtrados"),
    download_plot = list(en = "Download Plot (PNG)", zh = "\u4e0b\u8f7d\u56fe\u8868 (PNG)", pt = "Baixar Gr\u00e1fico (PNG)"),
    download_comparison = list(en = "Download Comparison Plot (PNG)", zh = "\u4e0b\u8f7d\u5bf9\u6bd4\u56fe (PNG)", pt = "Baixar Gr\u00e1fico de Compara\u00e7\u00e3o (PNG)"),
    
    # Results titles
    removed_records = list(en = "Summary of Removed Records per Column", zh = "\u6bcf\u5217\u79fb\u9664\u8bb0\u5f55\u6c47\u603b", pt = "Resumo de Registros Removidos por Coluna"),
    comparison_means = list(en = "Comparison of Means and Standard Deviations (Pre vs Post QC)", zh = "\u5747\u503c\u548c\u6807\u51c6\u5dee\u5bf9\u6bd4\uff08\u8d28\u63a7\u524d\u540e\uff09", pt = "Compara\u00e7\u00e3o de M\u00e9dias e Desvios Padr\u00e3o (Pr\u00e9 vs P\u00f3s QC)"),
    
    # Data summary
    data_summary = list(en = "Data Summary:", zh = "\u6570\u636e\u6458\u8981\uff1a", pt = "Resumo dos Dados:"),
    original_dataset = list(en = "Original dataset:", zh = "\u539f\u59cb\u6570\u636e\u96c6\uff1a", pt = "Conjunto de dados original:"),
    after_filtering = list(en = "After categorical filtering:", zh = "\u5206\u7c7b\u8fc7\u6ee4\u540e\uff1a", pt = "Ap\u00f3s filtragem categ\u00f3rica:"),
    filtered_out = list(en = "Filtered out:", zh = "\u5df2\u8fc7\u6ee4\uff1a", pt = "Filtrado:"),
    rows = list(en = "rows", zh = "\u884c", pt = "linhas"),
    
    # Filter information
    filter_by = list(en = "Filter by:", zh = "\u6309\u4ee5\u4e0b\u6761\u4ef6\u8fc7\u6ee4\uff1a", pt = "Filtrar por:"),
    no_categorical = list(en = "No categorical variables selected.", zh = "\u672a\u9009\u62e9\u5206\u7c7b\u53d8\u91cf\u3002", pt = "Nenhuma vari\u00e1vel categ\u00f3rica selecionada."),
    
    # Missing value modal
    missing_value_modal_title = list(en = "Download Options", zh = "\u4e0b\u8f7d\u9009\u9879", pt = "Op\u00e7\u00f5es de Download"),
    missing_value_modal_text = list(en = "Choose the format for missing values in the downloaded file:", zh = "\u9009\u62e9\u4e0b\u8f7d\u6587\u4ef6\u4e2d\u7f3a\u5931\u503c\u7684\u683c\u5f0f\uff1a", pt = "Escolha o formato para valores ausentes no arquivo baixado:"),
    missing_value_format_label = list(en = "Define Missing Values:", zh = "\u5b9a\u4e49\u7f3a\u5931\u503c\uff1a", pt = "Definir Valores Ausentes:"),
    missing_value_format_help = list(en = "Select which values should be treated as missing values when reading the data (multiple selection supported)", zh = "\u9009\u62e9\u5728\u8bfb\u53d6\u6570\u636e\u65f6\u5e94\u8be5\u88ab\u5f53\u4f5c\u7f3a\u5931\u503c\u5904\u7406\u7684\u503c\uff08\u652f\u6301\u591a\u9009\uff09", pt = "Selecione quais valores devem ser tratados como valores ausentes ao ler os dados (suporte a m\u00faltipla sele\u00e7\u00e3o)"),
    confirm_download_text = list(en = "Download", zh = "\u4e0b\u8f7d", pt = "Baixar"),
    cancel_download_text = list(en = "Cancel", zh = "\u53d6\u6d88", pt = "Cancelar"),

    # Error messages
    no_data_plot = list(en = "No numeric data available for plotting.", zh = "\u6ca1\u6709\u53ef\u7528\u4e8e\u7ed8\u56fe\u7684\u6570\u636e\u3002", pt = "Nenhum dado num\u00e9rico dispon\u00edvel para plotagem."),
    no_data_comparison = list(en = "No data available for comparison plots.", zh = "\u6ca1\u6709\u53ef\u7528\u4e8e\u5bf9\u6bd4\u56fe\u7684\u6570\u636e\u3002", pt = "Nenhum dado dispon\u00edvel para gr\u00e1ficos de compara\u00e7\u00e3o."),
    no_data_download = list(en = "No data available to download.", zh = "\u6ca1\u6709\u53ef\u4e0b\u8f7d\u7684\u6570\u636e\u3002", pt = "Nenhum dado dispon\u00edvel para download."),
    file_error = list(en = "Error reading file. Please check the file format.", zh = "\u8bfb\u53d6\u6587\u4ef6\u9519\u8bef\u3002\u8bf7\u68c0\u67e5\u6587\u4ef6\u683c\u5f0f\u3002", pt = "Erro ao ler arquivo. Verifique o formato do arquivo."),
    unsupported_file = list(en = "Unsupported file type:", zh = "\u4e0d\u652f\u6301\u7684\u6587\u4ef6\u7c7b\u578b\uff1a", pt = "Tipo de arquivo n\u00e3o suportado:")
  )
  labels[[key]][[lang]] %||% labels[[key]][["en"]]
}