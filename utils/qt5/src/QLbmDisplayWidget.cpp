/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "QLbmDisplayWidget.h"

#include <QPainter>
#include <QPaintEvent>

QLbmDisplayWidget::QLbmDisplayWidget(int width, int height, QWidget *parent)
  : QLbmWidget(parent)
  , m_image(width, height, QImage::Format_RGB32)
  , m_displayWidth(width)
  , m_displayHeight(height) {
  m_image.fill(Qt::black);
  setFixedSize(width, height);
}

QSize QLbmDisplayWidget::sizeHint() const {
  return QSize(m_displayWidth, m_displayHeight);
}

void QLbmDisplayWidget::setImage(const QImage &img) {
  m_image = img.convertToFormat(QImage::Format_RGB32);
  repaint();
}

void QLbmDisplayWidget::setImageAt(int x, int y, const QImage &img) {
  QPainter painter(&m_image);
  painter.drawImage(x, y, img.convertToFormat(QImage::Format_RGB32));
  painter.end();
  repaint();
}

void QLbmDisplayWidget::paintEvent(QPaintEvent *) {
  QPainter painter(this);
  painter.drawImage(0, 0, m_image);
}
