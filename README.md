# dataprevieweR <img src="https://github.com/user-attachments/assets/45bfe2ec-f067-4782-bddb-0e6e4d30ea14" align="right" width="120"/>

[![R-CMD-check](https://github.com/Thymine2001/dataprevieweR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thymine2001/dataprevieweR/actions)

**dataprevieweR** is an interactive R Shiny application built with the [golem](https://github.com/ThinkR-open/golem) framework for quick data previewing and quality control (QC).  
It allows you to load tabular datasets, explore their structure, filter observations, and visualize distributions in real time.

---
## 📊 Function Preview
![1bg88-cdgof](https://github.com/user-attachments/assets/da7ff150-084f-4b3f-81d0-e9f02f1d5416)


## ✨ Features
- 📂 **Data preview** – Upload or read a local csv, txt, xlsx, tsv etal file and instantly view the table  
- 🔍 **Filtering tools** – Apply thresholds or mean ± SD criteria to filter records  
- 📊 **Visualization** – Histogram and boxplot options for quick exploration  
- ⚙️ **Interactive workflow** – Adjust QC parameters and see results immediately  
- 💡 **Extensible** – Built with golem, easy to extend with new modules  
## 🌐 Online Version
Try it now ！！！: [https://vb6clt-huangyi-tang.shinyapps.io/datapreviewer/](https://vb6clt-huangyi-tang.shinyapps.io/datapreviewer/)

---

## 🛠️ Installation

Install the development version from GitHub

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("Thymine2001/dataprevieweR")
```

Install from a local source tarball

``` r
setwd("C:/Users/YourPath/")
remotes::install_local("dataprevieweR_0.0.0.9000.tar.gz", dependencies = TRUE)
```

## 🚀 Usage

After installation, load the package and run the app:

``` r
library(dataprevieweR)
dataprevieweR::run_app()
```

A Shiny app will launch in your default browser, allowing interactive
data exploration.

## 📦 Dependencies

The package will install the following key dependencies automatically: -
shiny - golem - config - DT - dplyr - ggplot2

## 📖 Example Workflow

1.  Launch the app using run_app().
2.  Upload a dataset (CSV).
3.  Select columns of interest for previewing.
4.  Apply QC filters (thresholds or mean ± SD).
5.  Visualize pre- and post-filter distributions.

## 🤝 Contributing

Contributions are welcome! - Report issues via the Issues page. - SubGPL-3
pull requests to improve features or documentation.

If you use dataprevieweR in your research or projects, we'd love to hear
your feedback!

## 📜 License

This project is released under the GPL-3 License.

------------------------------------------------------------------------

# dataprevieweR 中文说明

dataprevieweR 是一个基于 golem 框架构建的交互式 R Shiny
应用，用于快速预览数据和质量控制 (QC)。
它可以加载表格数据集，探索其结构，筛选记录，并实时可视化分布。

## ✨ 功能特性

📂 数据预览 -- 上传或读取本地 CSV 文件并即时查看表格<br>
🔍 筛选工具 -- 选择特定列并应用阈值或均值 ± SD 条件过滤记录<br>
📊 可视化 -- 内置直方图和箱线图选项用于数据分布可视化<br>
⚙️ 交互式工作流 -- 在 Shiny 界面中调整 QC 参数并即时查看结果<br>
💡 可扩展性 -- 基于 Golem 开发，便于添加新功能<br>

## 🛠️ 安装

从 GitHub 安装开发版

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("Thymine2001/dataprevieweR")
```

从本地源文件安装

``` r
setwd("C:/Users/YourPath/")
remotes::install_local("dataprevieweR_0.0.0.9000.tar.gz", dependencies = TRUE)
```

## 🚀 使用方法

安装完成后，加载包并运行应用：

``` r
library(dataprevieweR)
dataprevieweR::run_app()
```

浏览器将启动一个 Shiny 应用，允许交互式数据探索。

## 📦 依赖

该包会自动安装以下主要依赖： - shiny - golem - config - DT - dplyr -
ggplot2

## 📖 示例工作流

1.  使用 run_app() 启动应用。
2.  上传数据集 (CSV)。
3.  选择需要预览的列。
4.  应用 QC 筛选条件（阈值或均值 ± SD）。
5.  可视化筛选前后的分布。

## 🤝 贡献

欢迎贡献！ - 通过 Issues 页面报告问题。 - 提交 pull requests
以改进功能或文档。

如果你在研究或项目中使用 dataprevieweR，我们非常期待你的反馈！

## 📜 许可证

该项目基于 GPL-3 许可证发布。
