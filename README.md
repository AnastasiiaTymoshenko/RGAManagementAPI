## Описание пакета и способа установки

RGAManagementAPI - это пакет, в котором реализованы функции управления пользователями в Google Analytics.

Пакет “RGAManagementAPI” использует функции пакетов [googleAnalyticsR](https://cran.r-project.org/web/packages/googleAnalyticsR/index.html "googleAnalyticsR") 
 и [googleAuthR](https://cran.r-project.org/web/packages/googleAuthR/index.html "googleAuthR") Марка Эдмондсона.

Для установки пакета необходимо запустить следующий код в RStudio.

```r
install.packages("googleAnalyticsR")
library("googleAnalyticsR")
install.packages("googleAuthR")
library("googleAuthR")
install.packages("devtools")
library(devtools)
install_github("AnastasiiaTymoshenko/RGAManagementAPI")
library("RGAManagementAPI")
```

Для использования функций пакета необходимо пройти авторизацию. 

Для этого в переменную GA_AUTH_FILE сохраните путь к файлу с реквизитами доступа. Данный файл можно сгенерировать в консоли разработчика.

```r
GA_AUTH_FILE <- "C:/Users/user/Desktop/eswlgijsgljrsldg.apps.googleusercontent.com.json"
ga_auth(token = NULL, new_user = FALSE, no_auto = FALSE)
```


### Описание функций пакета

**delete_users_permissions()**

Функция delete_users_permissions() принимает список электронных почт пользователей, для которых необходимо закрыть доступ к аккаунтам Google Analytics на всех уровнях иерархии. Скрипт проходит по всем аккаунтам, ресурсам и представлениям, к которым есть доступ у почты, для которой вы провели авторизацию, и в случае если у вас есть право на управление пользователями, закрывает доступ для указанного списка электронных почт. В случае, если у вас нет прав на управление пользователей, выводится уведомление “Недостаточно прав для просмотра и удаления пользователей” и название аккаунта/ресурса/представления, для которого это справедливо.

**Пример использования
**

```r
install.packages("googleAnalyticsR")
library("googleAnalyticsR")
install.packages("googleAuthR")
library("googleAuthR")
install.packages("devtools")
library(devtools)
install_github("AnastasiiaTymoshenko/RGAManagementAPI")
library("RGAManagementAPI")

GA_AUTH_FILE <- "C:/Users/user/Desktop/eswlgijsgljrsldg.apps.googleusercontent.com.json"
ga_auth(token = NULL, new_user = FALSE, no_auto = FALSE)

emails_to_delete <- list("email_1@gmail.com", "email_2@gmail.com") 

delete_users_permissions(emails_to_delete)
```


