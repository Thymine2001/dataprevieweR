# 交互式部署脚本
library(rsconnect)

cat("=== dataprevieweR 部署到 shinyapps.io ===\n\n")

# 检查是否已经配置账户
if (length(rsconnect::accounts()) == 0) {
  cat("❌ 未检测到 shinyapps.io 账户配置\n")
  cat("请先完成以下步骤：\n")
  cat("1. 访问 https://www.shinyapps.io/ 注册账户\n")
  cat("2. 在账户设置中获取 Token 和 Secret\n")
  cat("3. 运行以下命令配置账户：\n\n")
  cat("rsconnect::setAccountInfo(\n")
  cat("  name='your-username',\n")
  cat("  token='your-token',\n")
  cat("  secret='your-secret'\n")
  cat(")\n\n")
  cat("配置完成后，请重新运行此脚本。\n")
} else {
  cat("✅ 检测到已配置的账户：\n")
  print(rsconnect::accounts())
  cat("\n")
  
  # 询问是否继续部署
  response <- readline("是否继续部署？(y/n): ")
  
  if (tolower(response) %in% c("y", "yes", "是")) {
    cat("\n开始部署...\n")
    
    tryCatch({
      rsconnect::deployApp(
        appDir = ".",
        appFiles = c("app.R", "R/", "inst/", "man/", "DESCRIPTION", "NAMESPACE"),
        appName = "dataprevieweR",
        appTitle = "Data Preview and QC Tool",
        forceUpdate = TRUE
      )
      cat("\n✅ 部署成功！\n")
      cat("您的应用已部署到 shinyapps.io\n")
    }, error = function(e) {
      cat("\n❌ 部署失败：\n")
      cat(as.character(e), "\n")
    })
  } else {
    cat("部署已取消。\n")
  }
}
