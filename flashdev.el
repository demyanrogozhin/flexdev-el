;;; flashdev.el --- Flash development environment

;; Author: Demyan Rogozhin <demyan.rogozhin@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup flashdev nil
  "Flash/Flex development environment"
  :group 'programming)

(defcustom flashdev-default-sdk ""
  "Path to default Flex SDK"
  :group 'flashdev)

;(require 'flashdev-ant)
(require 'flashdev-fcsh)

(provide 'flashdev)
;;; flashdev.el ends here
