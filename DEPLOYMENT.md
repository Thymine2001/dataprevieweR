# 部署到 shinyapps.io 指南

## 步骤 1: 安装 rsconnect 包
```r
install.packages("rsconnect")
```

## 步骤 2: 设置 shinyapps.io 账户
1. 访问 https://www.shinyapps.io/
2. 注册账户或登录
3. 在账户设置中获取 Token 和 Secret

## 步骤 3: 配置认证
在 R 中运行以下命令（替换为您的实际信息）：
```r
library(rsconnect)
rsconnect::setAccountInfo(
  name='your-username',
  token='your-token', 
  secret='your-secret'
)
```

## 步骤 4: 部署应用
```r
library(rsconnect)
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "R/", "inst/", "man/", "DESCRIPTION", "NAMESPACE"),
  appName = "dataprevieweR",
  appTitle = "Data Preview and QC Tool",
  forceUpdate = TRUE
)
```

## 注意事项
- 确保所有依赖都在 DESCRIPTION 文件中正确列出
- 应用名称必须是唯一的
- 首次部署可能需要几分钟时间
- 免费账户有使用限制

## 文件说明
- `app.R`: 应用入口点
- `deploy.R`: 部署脚本
- `R/`: R 源码文件
- `inst/`: 应用资源文件
- `man/`: 文档文件
