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

#include "QLbmQuickDisplayItem.h"
#include <QPainter>

QLbmQuickDisplayItem::QLbmQuickDisplayItem(int displayWidth, int displayHeight,
                                           QQuickItem *parent)
  : QQuickPaintedItem(parent)
  , m_image(displayWidth, displayHeight, QImage::Format_RGB32)
  , m_displayWidth(displayWidth)
  , m_displayHeight(displayHeight) {
  m_image.fill(Qt::black);
  setImplicitWidth(displayWidth);
  setImplicitHeight(displayHeight);
  setFillColor(Qt::black);
}

QSizeF QLbmQuickDisplayItem::sizeHint() const {
  return QSizeF(m_displayWidth, m_displayHeight);
}

void QLbmQuickDisplayItem::setImage(const QImage &img) {
  m_image = img.scaled(m_displayWidth, m_displayHeight);
  update();
}

void QLbmQuickDisplayItem::setImageAt(int x, int y, const QImage &img) {
  QPainter p(&m_image);
  p.drawImage(x, y, img);
  update();
}

void QLbmQuickDisplayItem::paint(QPainter *painter) {
  painter->drawImage(QRectF(0, 0, width(), height()), m_image);
}
